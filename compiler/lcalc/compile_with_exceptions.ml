(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

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
open Shared_ast
module D = Dcalc.Ast
module A = Ast

type 'm ctx = ('m D.expr, 'm A.expr Var.t) Var.Map.t
(** This environment contains a mapping between the variables in Dcalc and their
    correspondance in Lcalc. *)

let thunk_expr (type m) (e : m A.expr Bindlib.box) : m A.expr Bindlib.box =
  let dummy_var = Var.make "_" in
  let pos = Expr.pos (Bindlib.unbox e) in
  let arg_t = Marked.mark pos (TLit TUnit) in
  Expr.make_abs [| dummy_var |] e [arg_t] pos

let rec translate_default
    (ctx : 'm ctx)
    (exceptions : 'm D.expr list)
    (just : 'm D.expr)
    (cons : 'm D.expr)
    (mark_default : 'm mark) : 'm A.expr Bindlib.box =
  let exceptions =
    List.map (fun except -> thunk_expr (translate_expr ctx except)) exceptions
  in
  let pos = Expr.mark_pos mark_default in
  let exceptions =
    Expr.make_app
      (Expr.make_var
         ( Var.translate A.handle_default,
           Expr.with_ty mark_default (Utils.Marked.mark pos TAny) ))
      [
        Expr.earray exceptions mark_default;
        thunk_expr (translate_expr ctx just);
        thunk_expr (translate_expr ctx cons);
      ]
      pos
  in
  exceptions

and translate_expr (ctx : 'm ctx) (e : 'm D.expr) : 'm A.expr Bindlib.box =
  match Marked.unmark e with
  | EVar v -> Expr.make_var (Var.Map.find v ctx, Marked.get_mark e)
  | ETuple (args, s) ->
    Expr.etuple (List.map (translate_expr ctx) args) s (Marked.get_mark e)
  | ETupleAccess (e1, i, s, ts) ->
    Expr.etupleaccess (translate_expr ctx e1) i s ts (Marked.get_mark e)
  | EInj (e1, i, en, ts) ->
    Expr.einj (translate_expr ctx e1) i en ts (Marked.get_mark e)
  | EMatch (e1, cases, en) ->
    Expr.ematch (translate_expr ctx e1)
      (List.map (translate_expr ctx) cases)
      en (Marked.get_mark e)
  | EArray es ->
    Expr.earray (List.map (translate_expr ctx) es) (Marked.get_mark e)
  | ELit
      ((LBool _ | LInt _ | LRat _ | LMoney _ | LUnit | LDate _ | LDuration _) as
      l) ->
    Bindlib.box (Marked.same_mark_as (ELit l) e)
  | ELit LEmptyError -> Bindlib.box (Marked.same_mark_as (ERaise EmptyError) e)
  | EOp op -> Expr.eop op (Marked.get_mark e)
  | EIfThenElse (e1, e2, e3) ->
    Expr.eifthenelse (translate_expr ctx e1) (translate_expr ctx e2)
      (translate_expr ctx e3) (Marked.get_mark e)
  | EAssert e1 -> Expr.eassert (translate_expr ctx e1) (Marked.get_mark e)
  | ErrorOnEmpty arg ->
    Expr.ecatch (translate_expr ctx arg) EmptyError
      (Bindlib.box (Marked.same_mark_as (ERaise NoValueProvided) e))
      (Marked.get_mark e)
  | EApp (e1, args) ->
    Expr.eapp (translate_expr ctx e1)
      (List.map (translate_expr ctx) args)
      (Marked.get_mark e)
  | EAbs (binder, ts) ->
    let vars, body = Bindlib.unmbind binder in
    let ctx, lc_vars =
      Array.fold_right
        (fun var (ctx, lc_vars) ->
          let lc_var = Var.make (Bindlib.name_of var) in
          Var.Map.add var lc_var ctx, lc_var :: lc_vars)
        vars (ctx, [])
    in
    let lc_vars = Array.of_list lc_vars in
    let new_body = translate_expr ctx body in
    let new_binder = Bindlib.bind_mvar lc_vars new_body in
    Bindlib.box_apply
      (fun new_binder -> Marked.same_mark_as (EAbs (new_binder, ts)) e)
      new_binder
  | EDefault ([exn], just, cons) when !Cli.optimize_flag ->
    Expr.ecatch (translate_expr ctx exn) EmptyError
      (Expr.eifthenelse (translate_expr ctx just) (translate_expr ctx cons)
         (Bindlib.box (Marked.same_mark_as (ERaise EmptyError) e))
         (Marked.get_mark e))
      (Marked.get_mark e)
  | EDefault (exceptions, just, cons) ->
    translate_default ctx exceptions just cons (Marked.get_mark e)

let rec translate_scope_lets
    (decl_ctx : decl_ctx)
    (ctx : 'm ctx)
    (scope_lets : 'm D.expr scope_body_expr) :
    'm A.expr scope_body_expr Bindlib.box =
  match scope_lets with
  | Result e -> Bindlib.box_apply (fun e -> Result e) (translate_expr ctx e)
  | ScopeLet scope_let ->
    let old_scope_let_var, scope_let_next =
      Bindlib.unbind scope_let.scope_let_next
    in
    let new_scope_let_var = Var.make (Bindlib.name_of old_scope_let_var) in
    let new_scope_let_expr = translate_expr ctx scope_let.scope_let_expr in
    let new_ctx = Var.Map.add old_scope_let_var new_scope_let_var ctx in
    let new_scope_next = translate_scope_lets decl_ctx new_ctx scope_let_next in
    let new_scope_next = Bindlib.bind_var new_scope_let_var new_scope_next in
    Bindlib.box_apply2
      (fun new_scope_next new_scope_let_expr ->
        ScopeLet
          {
            scope_let_typ = scope_let.scope_let_typ;
            scope_let_kind = scope_let.scope_let_kind;
            scope_let_pos = scope_let.scope_let_pos;
            scope_let_next = new_scope_next;
            scope_let_expr = new_scope_let_expr;
          })
      new_scope_next new_scope_let_expr

let rec translate_scopes
    (decl_ctx : decl_ctx)
    (ctx : 'm ctx)
    (scopes : 'm D.expr scopes) : 'm A.expr scopes Bindlib.box =
  match scopes with
  | Nil -> Bindlib.box Nil
  | ScopeDef scope_def ->
    let old_scope_var, scope_next = Bindlib.unbind scope_def.scope_next in
    let new_scope_var =
      Var.make (Marked.unmark (ScopeName.get_info scope_def.scope_name))
    in
    let old_scope_input_var, scope_body_expr =
      Bindlib.unbind scope_def.scope_body.scope_body_expr
    in
    let new_scope_input_var = Var.make (Bindlib.name_of old_scope_input_var) in
    let new_ctx = Var.Map.add old_scope_input_var new_scope_input_var ctx in
    let new_scope_body_expr =
      translate_scope_lets decl_ctx new_ctx scope_body_expr
    in
    let new_scope_body_expr =
      Bindlib.bind_var new_scope_input_var new_scope_body_expr
    in
    let new_scope : 'm A.expr scope_body Bindlib.box =
      Bindlib.box_apply
        (fun new_scope_body_expr ->
          {
            scope_body_input_struct =
              scope_def.scope_body.scope_body_input_struct;
            scope_body_output_struct =
              scope_def.scope_body.scope_body_output_struct;
            scope_body_expr = new_scope_body_expr;
          })
        new_scope_body_expr
    in
    let new_ctx = Var.Map.add old_scope_var new_scope_var new_ctx in
    let scope_next =
      Bindlib.bind_var new_scope_var
        (translate_scopes decl_ctx new_ctx scope_next)
    in
    Bindlib.box_apply2
      (fun new_scope scope_next ->
        ScopeDef
          {
            scope_name = scope_def.scope_name;
            scope_body = new_scope;
            scope_next;
          })
      new_scope scope_next

let translate_program (prgm : 'm D.program) : 'm A.program =
  {
    scopes =
      Bindlib.unbox (translate_scopes prgm.decl_ctx Var.Map.empty prgm.scopes);
    decl_ctx = prgm.decl_ctx;
  }
