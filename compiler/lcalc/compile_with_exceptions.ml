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

let thunk_expr (type m) (e : m A.expr boxed) : m A.expr boxed =
  let dummy_var = Var.make "_" in
  let pos = Expr.pos e in
  let arg_t = Marked.mark pos (TLit TUnit) in
  Expr.make_abs [| dummy_var |] e [arg_t] pos

let rec translate_default
    (ctx : 'm ctx)
    (exceptions : 'm D.expr list)
    (just : 'm D.expr)
    (cons : 'm D.expr)
    (mark_default : 'm mark) : 'm A.expr boxed =
  let exceptions =
    List.map (fun except -> thunk_expr (translate_expr ctx except)) exceptions
  in
  let pos = Expr.mark_pos mark_default in
  let exceptions =
    Expr.make_app
      (Expr.make_var
         (Var.translate A.handle_default)
         (Expr.with_ty mark_default (Utils.Marked.mark pos TAny)))
      [
        Expr.earray exceptions mark_default;
        thunk_expr (translate_expr ctx just);
        thunk_expr (translate_expr ctx cons);
      ]
      pos
  in
  exceptions

and translate_expr (ctx : 'm ctx) (e : 'm D.expr) : 'm A.expr boxed =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EVar v -> Expr.make_var (Var.Map.find v ctx) m
  | EStruct { name; fields } ->
    Expr.estruct name (StructField.Map.map (translate_expr ctx) fields) m
  | EStructAccess { name; e; field } ->
    Expr.estructaccess (translate_expr ctx e) field name m
  | EInj { name; e; cons } -> Expr.einj (translate_expr ctx e) cons name m
  | EMatch { name; e; cases } ->
    Expr.ematch (translate_expr ctx e) name
      (EnumConstructor.Map.map (translate_expr ctx) cases)
      m
  | EArray es -> Expr.earray (List.map (translate_expr ctx) es) m
  | ELit
      ((LBool _ | LInt _ | LRat _ | LMoney _ | LUnit | LDate _ | LDuration _) as
      l) ->
    Expr.elit l m
  | ELit LEmptyError -> Expr.eraise EmptyError m
  | EOp op -> Expr.eop op m
  | EIfThenElse { cond; etrue; efalse } ->
    Expr.eifthenelse (translate_expr ctx cond) (translate_expr ctx etrue)
      (translate_expr ctx efalse)
      m
  | EAssert e1 -> Expr.eassert (translate_expr ctx e1) m
  | EErrorOnEmpty arg ->
    Expr.ecatch (translate_expr ctx arg) EmptyError
      (Expr.eraise NoValueProvided m)
      m
  | EApp { f; args } ->
    Expr.eapp (translate_expr ctx f)
      (List.map (translate_expr ctx) args)
      (Marked.get_mark e)
  | EAbs { binder; tys } ->
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
    let new_binder = Expr.bind lc_vars new_body in
    Expr.eabs new_binder tys (Marked.get_mark e)
  | EDefault { excepts = [exn]; just; cons } when !Cli.optimize_flag ->
    (* FIXME: bad place to rely on a global flag *)
    Expr.ecatch (translate_expr ctx exn) EmptyError
      (Expr.eifthenelse (translate_expr ctx just) (translate_expr ctx cons)
         (Expr.eraise EmptyError (Marked.get_mark e))
         (Marked.get_mark e))
      (Marked.get_mark e)
  | EDefault { excepts; just; cons } ->
    translate_default ctx excepts just cons (Marked.get_mark e)

let rec translate_scope_lets
    (decl_ctx : decl_ctx)
    (ctx : 'm ctx)
    (scope_lets : 'm D.expr scope_body_expr) :
    'm A.expr scope_body_expr Bindlib.box =
  match scope_lets with
  | Result e ->
    Bindlib.box_apply (fun e -> Result e) (Expr.Box.lift (translate_expr ctx e))
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
      new_scope_next
      (Expr.Box.lift new_scope_let_expr)

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
