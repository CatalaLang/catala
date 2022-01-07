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
    (fun (acc : expr Pos.marked) arg ->
      match Pos.unmark arg with
      | ELit (LBool true) -> acc
      | _ -> (EApp ((EOp (Binop And), pos), [ arg; acc ]), pos))
    acc list

let disjunction (args : expr Pos.marked list) (pos : Pos.t) =
  let acc, list = match args with hd :: tl -> (hd, tl) | [] -> ((ELit (LBool false), pos), []) in
  List.fold_left
    (fun (acc : expr Pos.marked) arg ->
      match Pos.unmark arg with
      | ELit (LBool false) -> acc
      | _ -> (EApp ((EOp (Binop Or), pos), [ arg; acc ]), pos))
    acc list

let rec generate_vc_must_not_return_empty (ctx : decl_ctx) (e : expr Pos.marked) : expr Pos.marked =
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
        let _, body = Bindlib.unmbind (Pos.unmark binder) in
        (generate_vc_must_not_return_empty ctx) body
    | EApp (f, args) ->
        conjunction
          (List.map (generate_vc_must_not_return_empty ctx) (f :: args))
          (Pos.get_position e)
    | EIfThenElse (e1, e2, e3) ->
        conjunction
          (List.map (generate_vc_must_not_return_empty ctx) [ e1; e2; e3 ])
          (Pos.get_position e)
    | ELit LEmptyError -> Pos.same_pos_as (ELit (LBool false)) e
    | EVar _ | ELit _ | EOp _ -> Pos.same_pos_as (ELit (LBool true)) e
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
     (Format.asprintf ">>> Input:@\n%a@\nOutput:@\n%a" (Print.format_expr ctx) e
        (Print.format_expr ctx) out); *)
  out
  [@@ocamlformat "wrap-comments=false"]

let generate_verification_conditions (p : program) : expr Pos.marked list =
  List.fold_left
    (fun acc (_s_name, _s_var, s_body) ->
      List.fold_left
        (fun acc s_let ->
          match s_let.scope_let_kind with
          | ScopeVarDefinition | SubScopeVarDefinition ->
              generate_vc_must_not_return_empty p.decl_ctx (Bindlib.unbox s_let.scope_let_expr)
              :: acc
          | _ -> acc)
        acc s_body.scope_body_lets)
    [] p.scopes
