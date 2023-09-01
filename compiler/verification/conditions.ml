(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët
   <alain.delaet--tixeuil@inria.fr>, Aymeric Fromherz
   <aymeric.fromherz@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
open Shared_ast
open Dcalc
open Ast

(** {1 Helpers and type definitions}*)

type vc_return = typed expr
(** The return type of VC generators is the VC expression *)

type ctx = {
  current_scope_name : ScopeName.t;
  decl : decl_ctx;
  input_vars : typed expr Var.t list;
  scope_variables_typs : (typed expr, typ) Var.Map.t;
}

let rec conjunction_exprs (exprs : typed expr list) (mark : typed mark) :
    typed expr =
  match exprs with
  | [] -> ELit (LBool true), mark
  | hd :: tl ->
    ( EApp
        {
          f =
            ( EOp
                {
                  op = And;
                  tys = [TLit TBool, Expr.pos hd; TLit TBool, Expr.pos hd];
                },
              mark );
          args = [hd; conjunction_exprs tl mark];
        },
      mark )

let conjunction (args : vc_return list) (mark : typed mark) : vc_return =
  let acc, list =
    match args with hd :: tl -> hd, tl | [] -> (ELit (LBool true), mark), []
  in
  List.fold_left
    (fun acc arg ->
      ( EApp
          {
            f =
              ( EOp
                  {
                    op = And;
                    tys = [TLit TBool, Expr.pos acc; TLit TBool, Expr.pos arg];
                  },
                mark );
            args = [arg; acc];
          },
        mark ))
    acc list

let negation (arg : vc_return) (mark : typed mark) : vc_return =
  ( EApp
      {
        f = EOp { op = Not; tys = [TLit TBool, Expr.pos arg] }, mark;
        args = [arg];
      },
    mark )

let disjunction (args : vc_return list) (mark : typed mark) : vc_return =
  let acc, list =
    match args with hd :: tl -> hd, tl | [] -> (ELit (LBool false), mark), []
  in
  List.fold_left
    (fun (acc : vc_return) arg ->
      ( EApp
          {
            f =
              ( EOp
                  {
                    op = Or;
                    tys = [TLit TBool, Expr.pos acc; TLit TBool, Expr.pos arg];
                  },
                mark );
            args = [arg; acc];
          },
        mark ))
    acc list

(** [half_product [a1,...,an] [b1,...,bm] returns [(a1,b1),...(a1,bn),...(an,b1),...(an,bm)]] *)
let half_product (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  l1
  |> List.mapi (fun i ei ->
         List.filteri (fun j _ -> i < j) l2 |> List.map (fun ej -> ei, ej))
  |> List.concat

(** This code skims through the topmost layers of the terms like this:
    [log (error_on_empty < reentrant_variable () | true :- e1 >)] for scope
    variables, or [fun () -> e1] for subscope variables. But what we really want
    to analyze is only [e1], so we match this outermost structure explicitely
    and have a clean verification condition generator that only runs on [e1] *)
let match_and_ignore_outer_reentrant_default (ctx : ctx) (e : typed expr) :
    typed expr =
  match Mark.remove e with
  | EErrorOnEmpty
      ( EDefault
          {
            excepts = [(EApp { f = EVar x, _; args = [(ELit LUnit, _)] }, _)];
            just = ELit (LBool true), _;
            cons;
          },
        _ )
    when List.exists (fun x' -> Var.equal x x') ctx.input_vars ->
    (* scope variables*)
    cons
  | EAbs { binder; tys = [(TLit TUnit, _)] } ->
    (* context sub-scope variables *)
    let _, body = Bindlib.unmbind binder in
    body
  | EAbs { binder; _ } -> (
    (* context scope variables *)
    let _, body = Bindlib.unmbind binder in
    match Mark.remove body with
    | EErrorOnEmpty e -> e
    | _ ->
      Message.raise_spanned_error (Expr.pos e)
        "Internal error: this expression does not have the structure expected \
         by the VC generator:\n\
         %a"
        (Print.expr ()) e)
  | EErrorOnEmpty d ->
    d (* input subscope variables and non-input scope variable *)
  | _ ->
    Message.raise_spanned_error (Expr.pos e)
      "Internal error: this expression does not have the structure expected by \
       the VC generator:\n\
       %a"
      (Print.expr ()) e

(** {1 Verification conditions generator}*)

(** [generate_vc_must_not_return_empty e] returns the dcalc boolean expression
    [b] such that if [b] is true, then [e] will never return an empty error. It
    also returns a map of all the types of locally free variables inside the
    expression. *)
let rec generate_vc_must_not_return_empty (ctx : ctx) (e : typed expr) :
    vc_return =
  match Mark.remove e with
  | EAbs { binder; _ } ->
    (* Hot take: for a function never to return an empty error when called, it
       has to do so whatever its input. So we universally quantify over the
       variable of the function when inspecting the body, resulting in simply
       traversing through in the code here. *)
    let _vars, body = Bindlib.unmbind binder in
    (generate_vc_must_not_return_empty ctx) body
  | EDefault { excepts; just; cons } ->
    (* <e1 ... en | ejust :- econs > never returns empty if and only if: - first
       we look if e1 .. en ejust can return empty; - if no, we check that if
       ejust is true, whether econs can return empty. *)
    disjunction
      (List.map (generate_vc_must_not_return_empty ctx) excepts
      @ [
          conjunction
            [
              generate_vc_must_not_return_empty ctx just;
              (let vc_just_expr = generate_vc_must_not_return_empty ctx cons in
               ( EIfThenElse
                   {
                     cond = just;
                     (* Comment from Alain: the justification is not checked for
                        holding an default term. In such cases, we need to
                        encode the logic of the default terms within the
                        generation of the verification condition
                        (Z3encoding.translate_expr). Answer from Denis:
                        Normally, there is a structural invariant from the
                        surface language to intermediate representation
                        translation preventing any default terms to appear in
                        justifications.*)
                     etrue = vc_just_expr;
                     efalse = ELit (LBool false), Mark.get e;
                   },
                 Mark.get e ));
            ]
            (Mark.get e);
        ])
      (Mark.get e)
  | EEmptyError -> Mark.copy e (ELit (LBool false))
  | EVar _
  (* Per default calculus semantics, you cannot call a function with an argument
     that evaluates to the empty error. Thus, all variable evaluate to
     non-empty-error terms. *)
  | ELit _ | EOp _ ->
    Mark.copy e (ELit (LBool true))
  | EApp { f; args } ->
    (* Invariant: For the [EApp] case, we assume here that function calls never
       return empty error, which implies all functions have been checked never
       to return empty errors. *)
    conjunction
      (generate_vc_must_not_return_empty ctx f
      :: List.flatten
           (List.map
              (fun arg ->
                match Mark.remove arg with
                | EStruct { fields; _ } ->
                  List.map
                    (fun field ->
                      match Mark.remove field with
                      | EAbs { binder; tys = [(TLit TUnit, _)] } -> (
                        (* Invariant: when calling a function with a thunked
                           emptyerror, this means we're in a direct scope call
                           with a context argument. In that case, we don't apply
                           the standard [EAbs] rule and suppose, in coherence
                           with the [EApp] invariant, that the subscope will
                           never return empty error so the thunked emptyerror
                           can be ignored *)
                        let _vars, body = Bindlib.unmbind binder in
                        match Mark.remove body with
                        | EEmptyError -> Mark.copy field (ELit (LBool true))
                        | _ ->
                          (* same as basic [EAbs case]*)
                          generate_vc_must_not_return_empty ctx field)
                      | _ -> generate_vc_must_not_return_empty ctx field)
                    (StructField.Map.values fields)
                | _ -> [generate_vc_must_not_return_empty ctx arg])
              args))
      (Mark.get e)
  | _ ->
    conjunction
      (Expr.shallow_fold
         (fun e acc -> generate_vc_must_not_return_empty ctx e :: acc)
         e [])
      (Mark.get e)

(** [generate_vc_must_not_return_conflict e] returns the dcalc boolean
    expression [b] such that if [b] is true, then [e] will never return a
    conflict error. It also returns a map of all the types of locally free
    variables inside the expression. *)
let rec generate_vc_must_not_return_conflict (ctx : ctx) (e : typed expr) :
    vc_return =
  (* See the code of [generate_vc_must_not_return_empty] for a list of
     invariants on which this function relies on. *)
  match Mark.remove e with
  | EAbs { binder; _ } ->
    let _vars, body = Bindlib.unmbind binder in
    (generate_vc_must_not_return_conflict ctx) body
  | EVar _ | ELit _ | EOp _ -> Mark.copy e (ELit (LBool true))
  | EDefault { excepts; just; cons } ->
    (* <e1 ... en | ejust :- econs > never returns conflict if and only if: -
       neither e1 nor ... nor en nor ejust nor econs return conflict - there is
       no two differents ei ej that are not empty. *)
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
                  (Mark.get e))
              (half_product excepts excepts))
           (Mark.get e))
        (Mark.get e)
    in
    let others =
      List.map
        (generate_vc_must_not_return_conflict ctx)
        (just :: cons :: excepts)
    in
    let out = conjunction (quadratic :: others) (Mark.get e) in
    out
  | _ ->
    conjunction
      (Expr.shallow_fold
         (fun e acc -> generate_vc_must_not_return_conflict ctx e :: acc)
         e [])
      (Mark.get e)

(** {1 Interface}*)

type verification_condition_kind = NoEmptyError | NoOverlappingExceptions

type verification_condition = {
  vc_guard : typed expr;
  (* should have type bool *)
  vc_kind : verification_condition_kind;
  (* All assertions defined at the top-level of the scope corresponding to this
     assertion *)
  vc_asserts : typed expr;
  vc_scope : ScopeName.t;
  vc_variable : typed expr Var.t Mark.pos;
}

let trivial_assert e = Mark.copy e (ELit (LBool true))

let rec generate_verification_conditions_scope_body_expr
    (ctx : ctx)
    (scope_body_expr : 'm expr scope_body_expr) :
    ctx * verification_condition list * typed expr list =
  match scope_body_expr with
  | Result _ -> ctx, [], []
  | ScopeLet scope_let ->
    let scope_let_var, scope_let_next =
      Bindlib.unbind scope_let.scope_let_next
    in
    let new_ctx, vc_list, assert_list =
      match scope_let.scope_let_kind with
      | Assertion -> (
        let e =
          Expr.unbox (Expr.remove_logging_calls scope_let.scope_let_expr)
        in
        match Mark.remove e with
        | EAssert e ->
          let e = match_and_ignore_outer_reentrant_default ctx e in
          ctx, [], [e]
        | _ ->
          Message.raise_spanned_error (Expr.pos e)
            "Internal error: this assertion does not have the structure \
             expected by the VC generator:\n\
             %a"
            (Print.expr ()) e)
      | DestructuringInputStruct ->
        { ctx with input_vars = scope_let_var :: ctx.input_vars }, [], []
      | ScopeVarDefinition | SubScopeVarDefinition ->
        (* For scope variables, we should check both that they never evaluate to
           emptyError nor conflictError. But for subscope variable definitions,
           what we're really doing is adding exceptions to something defined in
           the subscope so we just ought to verify only that the exceptions
           overlap. *)
        let e =
          Expr.unbox (Expr.remove_logging_calls scope_let.scope_let_expr)
        in
        let e = match_and_ignore_outer_reentrant_default ctx e in
        let vc_confl = generate_vc_must_not_return_conflict ctx e in
        let vc_confl =
          if Globals.optimize () then
            Expr.unbox
              (Shared_ast.Optimizations.optimize_expr ctx.decl vc_confl)
          else vc_confl
        in
        let vc_list =
          [
            {
              vc_guard = Mark.copy e (Mark.remove vc_confl);
              vc_kind = NoOverlappingExceptions;
              (* Placeholder until we add all assertions in scope once
               * we finished traversing it *)
              vc_asserts = trivial_assert e;
              vc_scope = ctx.current_scope_name;
              vc_variable = scope_let_var, scope_let.scope_let_pos;
            };
          ]
        in
        let vc_list =
          match scope_let.scope_let_kind with
          | ScopeVarDefinition ->
            let vc_empty = generate_vc_must_not_return_empty ctx e in
            let vc_empty =
              if Globals.optimize () then
                Expr.unbox
                  (Shared_ast.Optimizations.optimize_expr ctx.decl vc_empty)
              else vc_empty
            in
            {
              vc_guard = Mark.copy e (Mark.remove vc_empty);
              vc_kind = NoEmptyError;
              vc_asserts = trivial_assert e;
              vc_scope = ctx.current_scope_name;
              vc_variable = scope_let_var, scope_let.scope_let_pos;
            }
            :: vc_list
          | _ -> vc_list
        in
        ctx, vc_list, []
      | _ -> ctx, [], []
    in
    let new_ctx, new_vcs, new_asserts =
      generate_verification_conditions_scope_body_expr
        {
          new_ctx with
          scope_variables_typs =
            Var.Map.add scope_let_var scope_let.scope_let_typ
              new_ctx.scope_variables_typs;
        }
        scope_let_next
    in
    new_ctx, vc_list @ new_vcs, assert_list @ new_asserts

let generate_verification_conditions_code_items
    (decl_ctx : decl_ctx)
    (code_items : 'm expr code_item_list)
    (s : ScopeName.t option) : verification_condition list =
  Scope.fold_left
    ~f:(fun vcs item _ ->
      match item with
      | Topdef _ -> []
      | ScopeDef (name, body) ->
        let is_selected_scope =
          match s with
          | Some s when ScopeName.equal s name -> true
          | None -> true
          | _ -> false
        in
        let new_vcs =
          if is_selected_scope then
            let _scope_input_var, scope_body_expr =
              Bindlib.unbind body.scope_body_expr
            in
            let ctx =
              {
                current_scope_name = name;
                decl = decl_ctx;
                input_vars = [];
                scope_variables_typs =
                  Var.Map.empty
                  (* We don't need to add the typ of the scope input var here
                     because it will never appear in an expression for which we
                     generate a verification conditions (the big struct is
                     destructured with a series of let bindings just after. )*);
              }
            in
            let _, vcs, asserts =
              generate_verification_conditions_scope_body_expr ctx
                scope_body_expr
            in
            let combined_assert =
              conjunction_exprs asserts
                (Typed
                   { pos = Pos.no_pos; ty = Mark.add Pos.no_pos (TLit TBool) })
            in
            List.map (fun vc -> { vc with vc_asserts = combined_assert }) vcs
          else []
        in
        new_vcs @ vcs)
    ~init:[] code_items

let generate_verification_conditions (p : 'm program) (s : ScopeName.t option) :
    verification_condition list =
  let vcs =
    generate_verification_conditions_code_items p.decl_ctx p.code_items s
  in
  (* We sort this list by scope name and then variable name to ensure consistent
     output for testing*)
  List.sort
    (fun vc1 vc2 ->
      let to_str vc =
        Format.asprintf "%s.%s"
          (Format.asprintf "%a" ScopeName.format vc.vc_scope)
          (Bindlib.name_of (Mark.remove vc.vc_variable))
      in
      String.compare (to_str vc1) (to_str vc2))
    vcs
