(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët
   <alain.delaet--tixeuil@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Utils
open Dcalc
open Ast

(** {1 Helpers and type definitions}*)

type vc_return = expr Pos.marked * typ Pos.marked VarMap.t
(** The return type of VC generators is the VC expression plus the types of any
    locally free variable inside that expression. *)

type ctx = { decl : decl_ctx; input_vars : Var.t list }

let conjunction (args : vc_return list) (pos : Pos.t) : vc_return =
  let acc, list =
    match args with
    | hd :: tl -> (hd, tl)
    | [] -> (((ELit (LBool true), pos), VarMap.empty), [])
  in
  List.fold_left
    (fun (acc, acc_ty) (arg, arg_ty) ->
      ( (EApp ((EOp (Binop And), pos), [ arg; acc ]), pos),
        VarMap.union (fun _ _ _ -> failwith "should not happen") acc_ty arg_ty
      ))
    acc list

let negation ((arg, arg_ty) : vc_return) (pos : Pos.t) : vc_return =
  ((EApp ((EOp (Unop Not), pos), [ arg ]), pos), arg_ty)

let disjunction (args : vc_return list) (pos : Pos.t) : vc_return =
  let acc, list =
    match args with
    | hd :: tl -> (hd, tl)
    | [] -> (((ELit (LBool false), pos), VarMap.empty), [])
  in
  List.fold_left
    (fun ((acc, acc_ty) : vc_return) (arg, arg_ty) ->
      ( (EApp ((EOp (Binop Or), pos), [ arg; acc ]), pos),
        VarMap.union (fun _ _ _ -> failwith "should not happen") acc_ty arg_ty
      ))
    acc list

(** [half_product \[a1,...,an\] \[b1,...,bm\] returns \[(a1,b1),...(a1,bn),...(an,b1),...(an,bm)\]] *)
let half_product (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  l1
  |> List.mapi (fun i ei ->
         List.filteri (fun j _ -> i < j) l2 |> List.map (fun ej -> (ei, ej)))
  |> List.concat

(** This code skims through the topmost layers of the terms like this:
    [log (error_on_empty < reentrant_variable () | true :- e1 >)] for scope
    variables, or [fun () -> e1] for subscope variables. But what we really want
    to analyze is only [e1], so we match this outermost structure explicitely
    and have a clean verification condition generator that only runs on [e1] *)
let match_and_ignore_outer_reentrant_default (ctx : ctx) (e : expr Pos.marked) :
    expr Pos.marked =
  match Pos.unmark e with
  | EApp
      ( (EOp (Unop (Log _)), _),
        [
          ( ErrorOnEmpty
              ( EDefault
                  ( [ (EApp ((EVar (x, _), _), [ (ELit LUnit, _) ]), _) ],
                    (ELit (LBool true), _),
                    cons ),
                _ ),
            _ );
        ] )
    when List.exists (fun x' -> Bindlib.eq_vars x x') ctx.input_vars ->
      (* scope variables*)
      cons
  | EAbs ((binder, _), [ (TLit TUnit, _) ]) -> (
      (* context sub-scope variables *)
      let _, body = Bindlib.unmbind binder in
      match Pos.unmark body with
      | EApp ((EOp (Unop (Log _)), _), [ arg ]) -> arg
      | _ ->
          Errors.raise_spanned_error
            (Format.asprintf
               "Internal error: this expression does not have the structure \
                expected by the VC generator:\n\
                %a"
               (Print.format_expr ~debug:true ctx.decl)
               e)
            (Pos.get_position e))
  | ErrorOnEmpty (EApp ((EOp (Unop (Log _)), _), [ d ]), _)
  | EApp ((EOp (Unop (Log _)), _), [ (ErrorOnEmpty d, _) ]) ->
      d (* input subscope variables and non-input scope variable *)
  | _ ->
      Errors.raise_spanned_error
        (Format.asprintf
           "Internal error: this expression does not have the structure \
            expected by the VC generator:\n\
            %a"
           (Print.format_expr ~debug:true ctx.decl)
           e)
        (Pos.get_position e)

(** {1 Verification conditions generator}*)

(** [generate_vc_must_not_return_empty e] returns the dcalc boolean expression
    [b] such that if [b] is true, then [e] will never return an empty error. It
    also returns a map of all the types of locally free variables inside the
    expression. *)
let rec generate_vc_must_not_return_empty (ctx : ctx) (e : expr Pos.marked) :
    vc_return =
  let out =
    match Pos.unmark e with
    | ETuple (args, _) | EArray args ->
        conjunction
          (List.map (generate_vc_must_not_return_empty ctx) args)
          (Pos.get_position e)
    | EMatch (arg, arms, _) ->
        conjunction
          (List.map (generate_vc_must_not_return_empty ctx) (arg :: arms))
          (Pos.get_position e)
    | ETupleAccess (e1, _, _, _)
    | EInj (e1, _, _, _)
    | EAssert e1
    | ErrorOnEmpty e1 ->
        (generate_vc_must_not_return_empty ctx) e1
    | EAbs (binder, typs) ->
        (* Hot take: for a function never to return an empty error when called, it has to do
           so whatever its input. So we universally quantify over the variable of the function
           when inspecting the body, resulting in simply traversing through in the code here. *)
        let vars, body = Bindlib.unmbind (Pos.unmark binder) in
        let vc_body_expr, vc_body_ty =
          (generate_vc_must_not_return_empty ctx) body
        in
        ( vc_body_expr,
          List.fold_left
            (fun acc (var, ty) -> VarMap.add var ty acc)
            vc_body_ty
            (List.map2 (fun x y -> (x, y)) (Array.to_list vars) typs) )
    | EApp (f, args) ->
        (* We assume here that function calls never return empty error, which implies
           all functions have been checked never to return empty errors. *)
        conjunction
          (List.map (generate_vc_must_not_return_empty ctx) (f :: args))
          (Pos.get_position e)
    | EIfThenElse (e1, e2, e3) ->
        let e1_vc, vc_typ1 = generate_vc_must_not_return_empty ctx e1 in
        let e2_vc, vc_typ2 = generate_vc_must_not_return_empty ctx e2 in
        let e3_vc, vc_typ3 = generate_vc_must_not_return_empty ctx e3 in
        conjunction
          [
            (e1_vc, vc_typ1);
            ( (EIfThenElse (e1, e2_vc, e3_vc), Pos.get_position e),
              VarMap.union
                (fun _ _ _ -> failwith "should not happen")
                vc_typ2 vc_typ3 );
          ]
          (Pos.get_position e)
    | ELit LEmptyError -> (Pos.same_pos_as (ELit (LBool false)) e, VarMap.empty)
    | EVar _
    (* Per default calculus semantics, you cannot call a function with an argument
       that evaluates to the empty error. Thus, all variable evaluate to non-empty-error terms. *)
    | ELit _ | EOp _ ->
        (Pos.same_pos_as (ELit (LBool true)) e, VarMap.empty)
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
                  (let vc_just_expr, vc_just_ty =
                     generate_vc_must_not_return_empty ctx cons
                   in
                   ( ( EIfThenElse
                         ( just,
                           (* Comment from Alain: the justification is not checked for holding an default term.
                              In such cases, we need to encode the logic of the default terms within
                              the generation of the verification condition (Z3encoding.translate_expr).
                              Answer from Denis: Normally, there is a structural invariant from the
                              surface language to intermediate representation translation preventing
                              any default terms to appear in justifications.*)
                           vc_just_expr,
                           (ELit (LBool false), Pos.get_position e) ),
                       Pos.get_position e ),
                     vc_just_ty ));
                ]
                (Pos.get_position e);
            ])
          (Pos.get_position e)
  in
  out
  [@@ocamlformat "wrap-comments=false"]

(** [generate_vs_must_not_return_confict e] returns the dcalc boolean expression
    [b] such that if [b] is true, then [e] will never return a conflict error.
    It also returns a map of all the types of locally free variables inside the
    expression. *)
let rec generate_vs_must_not_return_confict (ctx : ctx) (e : expr Pos.marked) :
    vc_return =
  let out =
    (* See the code of [generate_vc_must_not_return_empty] for a list of invariants on which this
       function relies on. *)
    match Pos.unmark e with
    | ETuple (args, _) | EArray args ->
        conjunction
          (List.map (generate_vs_must_not_return_confict ctx) args)
          (Pos.get_position e)
    | EMatch (arg, arms, _) ->
        conjunction
          (List.map (generate_vs_must_not_return_confict ctx) (arg :: arms))
          (Pos.get_position e)
    | ETupleAccess (e1, _, _, _)
    | EInj (e1, _, _, _)
    | EAssert e1
    | ErrorOnEmpty e1 ->
        generate_vs_must_not_return_confict ctx e1
    | EAbs (binder, typs) ->
        let vars, body = Bindlib.unmbind (Pos.unmark binder) in
        let vc_body_expr, vc_body_ty =
          (generate_vs_must_not_return_confict ctx) body
        in
        ( vc_body_expr,
          List.fold_left
            (fun acc (var, ty) -> VarMap.add var ty acc)
            vc_body_ty
            (List.map2 (fun x y -> (x, y)) (Array.to_list vars) typs) )
    | EApp (f, args) ->
        conjunction
          (List.map (generate_vs_must_not_return_confict ctx) (f :: args))
          (Pos.get_position e)
    | EIfThenElse (e1, e2, e3) ->
        let e1_vc, vc_typ1 = generate_vs_must_not_return_confict ctx e1 in
        let e2_vc, vc_typ2 = generate_vs_must_not_return_confict ctx e2 in
        let e3_vc, vc_typ3 = generate_vs_must_not_return_confict ctx e3 in
        conjunction
          [
            (e1_vc, vc_typ1);
            ( (EIfThenElse (e1, e2_vc, e3_vc), Pos.get_position e),
              VarMap.union
                (fun _ _ _ -> failwith "should not happen")
                vc_typ2 vc_typ3 );
          ]
          (Pos.get_position e)
    | EVar _ | ELit _ | EOp _ ->
        (Pos.same_pos_as (ELit (LBool true)) e, VarMap.empty)
    | EDefault (exceptions, just, cons) ->
        (* <e1 ... en | ejust :- econs > never returns conflict if and only if:
           - neither e1 nor ... nor en nor ejust nor econs return conflict
           - there is no two differents ei ej that are not empty. *)
        let quadratic =
          negation
            (disjunction
               (List.map
                  (fun (e1, e2) ->
                    conjunction
                      [
                        generate_vc_must_not_return_empty ctx e1;
                        generate_vc_must_not_return_empty ctx e2;
                      ]
                      (Pos.get_position e))
                  (half_product exceptions exceptions))
               (Pos.get_position e))
            (Pos.get_position e)
        in
        let others =
          List.map
            (generate_vs_must_not_return_confict ctx)
            (just :: cons :: exceptions)
        in
        let out = conjunction (quadratic :: others) (Pos.get_position e) in
        out
  in
  out
  [@@ocamlformat "wrap-comments=false"]

(** {1 Interface}*)

type verification_condition_kind = NoEmptyError | NoOverlappingExceptions

type verification_condition = {
  vc_guard : expr Pos.marked;
  (* should have type bool *)
  vc_kind : verification_condition_kind;
  vc_scope : ScopeName.t;
  vc_variable : Var.t Pos.marked;
  vc_free_vars_typ : typ Pos.marked VarMap.t;
}

let generate_verification_conditions (p : program) : verification_condition list
    =
  List.fold_left
    (fun acc (s_name, _s_var, s_body) ->
      let ctx = { decl = p.decl_ctx; input_vars = [] } in
      let acc, _ =
        List.fold_left
          (fun (acc, ctx) s_let ->
            match s_let.scope_let_kind with
            | DestructuringInputStruct ->
                ( acc,
                  {
                    ctx with
                    input_vars =
                      Pos.unmark s_let.scope_let_var :: ctx.input_vars;
                  } )
            | ScopeVarDefinition | SubScopeVarDefinition ->
                (* For scope variables, we should check both that they never
                   evaluate to emptyError nor conflictError. But for subscope
                   variable definitions, what we're really doing is adding
                   exceptions to something defined in the subscope so we just
                   ought to verify only that the exceptions overlap. *)
                let e =
                  match_and_ignore_outer_reentrant_default ctx
                    (Bindlib.unbox s_let.scope_let_expr)
                in
                let vc_confl, vc_confl_typs =
                  generate_vs_must_not_return_confict ctx e
                in
                let vc_confl =
                  if !Cli.optimize_flag then
                    Bindlib.unbox
                      (Optimizations.optimize_expr p.decl_ctx vc_confl)
                  else vc_confl
                in
                let vc_list =
                  [
                    {
                      vc_guard = Pos.same_pos_as (Pos.unmark vc_confl) e;
                      vc_kind = NoOverlappingExceptions;
                      vc_free_vars_typ = vc_confl_typs;
                      vc_scope = s_name;
                      vc_variable = s_let.scope_let_var;
                    };
                  ]
                in
                let vc_list =
                  match s_let.scope_let_kind with
                  | ScopeVarDefinition ->
                      let vc_empty, vc_empty_typs =
                        generate_vc_must_not_return_empty ctx e
                      in
                      let vc_empty =
                        if !Cli.optimize_flag then
                          Bindlib.unbox
                            (Optimizations.optimize_expr p.decl_ctx vc_empty)
                        else vc_empty
                      in
                      {
                        vc_guard = Pos.same_pos_as (Pos.unmark vc_empty) e;
                        vc_kind = NoEmptyError;
                        vc_free_vars_typ = vc_empty_typs;
                        vc_scope = s_name;
                        vc_variable = s_let.scope_let_var;
                      }
                      :: vc_list
                  | _ -> vc_list
                in
                (vc_list @ acc, ctx)
            | _ -> (acc, ctx))
          (acc, ctx) s_body.scope_body_lets
      in
      acc)
    [] p.scopes
