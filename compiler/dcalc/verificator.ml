(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2022 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils
open Ast

let conjunction (args : expr Pos.marked list) (pos : Pos.t) =
  let acc, list = match args with hd :: tl -> (hd, tl) | [] -> ((ELit (LBool true), pos), []) in
  List.fold_left
    (fun (acc : expr Pos.marked) arg -> (EApp ((EOp (Binop And), pos), [ arg; acc ]), pos))
    acc list

let disjunction (args : expr Pos.marked list) (pos : Pos.t) =
  let acc, list = match args with hd :: tl -> (hd, tl) | [] -> ((ELit (LBool false), pos), []) in
  List.fold_left
    (fun (acc : expr Pos.marked) arg -> (EApp ((EOp (Binop Or), pos), [ arg; acc ]), pos))
    acc list

type ctx = { decl : decl_ctx; input_vars : Var.t list }

(** [generate_vc_must_not_return_empty_e] returns the logical expression [b] such that if [b] is
    true, then [e] will never return an empty error. *)
let rec generate_vc_must_not_return_empty (ctx : ctx) (e : expr Pos.marked) : expr Pos.marked =
  let out =
    match Pos.unmark e with
    | ETuple (args, _) | EArray args ->
        conjunction (List.map (generate_vc_must_not_return_empty ctx) args) (Pos.get_position e)
    | EMatch (arg, arms, _) ->
        conjunction
          (List.map (generate_vc_must_not_return_empty ctx) (arg :: arms))
          (Pos.get_position e)
    | ETupleAccess (e1, _, _, _) | EInj (e1, _, _, _) | EAssert e1 | ErrorOnEmpty e1 ->
        (generate_vc_must_not_return_empty ctx) e1
    | EAbs (binder, _) ->
        (* Hot take: for a function never to return an empty error when called, it has to do
           so whatever its input. So we universally quantify over the variable of the function
           when inspecting the body, resulting in simply traversing through in the code here. *)
        let _, body = Bindlib.unmbind (Pos.unmark binder) in
        (generate_vc_must_not_return_empty ctx) body
    | EApp ((EVar (x, _), _), [ (ELit LUnit, _) ])
      when List.exists (fun x' -> Bindlib.eq_vars x x') ctx.input_vars ->
        (* Here we have a special case. If all the default trees in all the scopes of our
           program respect our verification, then there will be no empty error anywhere. Except at
           one place: when we don't pass a redefining argument to a callee scope. This matches
           the reentrant expressions in our default trees. *)
        Pos.same_pos_as (ELit (LBool false)) e
    | EApp (f, args) ->
        conjunction
          (List.map (generate_vc_must_not_return_empty ctx) (f :: args))
          (Pos.get_position e)
    | EIfThenElse (e1, e2, e3) ->
        conjunction
          (List.map (generate_vc_must_not_return_empty ctx) [ e1; e2; e3 ])
          (Pos.get_position e)
    | ELit LEmptyError -> Pos.same_pos_as (ELit (LBool false)) e
    | EVar _
    (* Per default calculus semantics, you cannot call a function with an argument
       that evaluates to the empty error. Thus, all variable evaluate to non-empty-error terms. *)
    | ELit _ | EOp _ ->
        Pos.same_pos_as (ELit (LBool true)) e
    | EDefault (exceptions, just, cons) ->
        (* <e1 ... en | ejust :- econs > never returns empty if and only if:
           - first we look if e1 .. en ejust can return empty;
           - if no, we check that if ejust is true, whether econs can return empty.
        *)
        disjunction
          (List.map (generate_vc_must_not_return_empty ctx) exceptions
          @ [
              conjunction
                [
                  generate_vc_must_not_return_empty ctx just;
                  ( EIfThenElse
                      ( just,
                        (generate_vc_must_not_return_empty ctx) cons,
                        (ELit (LBool false), Pos.get_position e) ),
                    Pos.get_position e );
                ]
                (Pos.get_position e);
            ])
          (Pos.get_position e)
  in
  (* Cli.debug_print
     (Format.asprintf ">>> Input:@\n%a@\nOutput:@\n%a" (Print.format_expr ctx.decl) e
        (Print.format_expr ctx.decl) out); *)
  out
  [@@ocamlformat "wrap-comments=false"]

let generate_verification_conditions (p : program) : expr Pos.marked list =
  List.fold_left
    (fun acc (_s_name, _s_var, s_body) ->
      let ctx = { decl = p.decl_ctx; input_vars = [] } in
      let acc, _ =
        List.fold_left
          (fun (acc, ctx) s_let ->
            match s_let.scope_let_kind with
            | DestructuringInputStruct ->
                (acc, { ctx with input_vars = Pos.unmark s_let.scope_let_var :: ctx.input_vars })
            | ScopeVarDefinition | SubScopeVarDefinition ->
                let e = Bindlib.unbox s_let.scope_let_expr in
                let vc = generate_vc_must_not_return_empty ctx e in
                let vc =
                  if !Cli.optimize_flag then Optimizations.optimize_expr p.decl_ctx vc else vc
                in
                (* TODO: drop logs for Aymeric *)
                (Pos.same_pos_as (Pos.unmark vc) e :: acc, ctx)
            | _ -> (acc, ctx))
          (acc, ctx) s_body.scope_body_lets
      in
      acc)
    [] p.scopes
