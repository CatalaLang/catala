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
module D = Dcalc.Ast
module A = Ast

type ctx = A.Var.t D.VarMap.t
(** This environment contains a mapping between the variables in Dcalc and their
    correspondance in Lcalc. *)

let translate_lit (l : D.lit) : A.expr =
  match l with
  | D.LBool l -> A.ELit (A.LBool l)
  | D.LInt i -> A.ELit (A.LInt i)
  | D.LRat r -> A.ELit (A.LRat r)
  | D.LMoney m -> A.ELit (A.LMoney m)
  | D.LUnit -> A.ELit A.LUnit
  | D.LDate d -> A.ELit (A.LDate d)
  | D.LDuration d -> A.ELit (A.LDuration d)
  | D.LEmptyError -> A.ERaise A.EmptyError

let thunk_expr (e : A.expr Pos.marked Bindlib.box) (pos : Pos.t) :
    A.expr Pos.marked Bindlib.box =
  let dummy_var = A.Var.make ("_", pos) in
  A.make_abs [| dummy_var |] e pos [D.TAny, pos] pos

let rec translate_default
    (ctx : ctx)
    (exceptions : D.expr Pos.marked list)
    (just : D.expr Pos.marked)
    (cons : D.expr Pos.marked)
    (pos_default : Pos.t) : A.expr Pos.marked Bindlib.box =
  let exceptions =
    List.map
      (fun except -> thunk_expr (translate_expr ctx except) pos_default)
      exceptions
  in
  let exceptions =
    A.make_app
      (A.make_var (A.handle_default, pos_default))
      [
        A.earray exceptions pos_default;
        thunk_expr (translate_expr ctx just) pos_default;
        thunk_expr (translate_expr ctx cons) pos_default;
      ]
      pos_default
  in
  exceptions

and translate_expr (ctx : ctx) (e : D.expr Pos.marked) :
    A.expr Pos.marked Bindlib.box =
  match Pos.unmark e with
  | D.EVar v -> A.make_var (D.VarMap.find (Pos.unmark v) ctx, Pos.get_position e)
  | D.ETuple (args, s) ->
    A.etuple (List.map (translate_expr ctx) args) s (Pos.get_position e)
  | D.ETupleAccess (e1, i, s, ts) ->
    A.etupleaccess (translate_expr ctx e1) i s ts (Pos.get_position e)
  | D.EInj (e1, i, en, ts) ->
    A.einj (translate_expr ctx e1) i en ts (Pos.get_position e)
  | D.EMatch (e1, cases, en) ->
    A.ematch (translate_expr ctx e1)
      (List.map (translate_expr ctx) cases)
      en (Pos.get_position e)
  | D.EArray es ->
    A.earray (List.map (translate_expr ctx) es) (Pos.get_position e)
  | D.ELit l -> Bindlib.box (Pos.same_pos_as (translate_lit l) e)
  | D.EOp op -> A.eop op (Pos.get_position e)
  | D.EIfThenElse (e1, e2, e3) ->
    A.eifthenelse (translate_expr ctx e1) (translate_expr ctx e2)
      (translate_expr ctx e3) (Pos.get_position e)
  | D.EAssert e1 -> A.eassert (translate_expr ctx e1) (Pos.get_position e)
  | D.ErrorOnEmpty arg ->
    A.ecatch (translate_expr ctx arg) A.EmptyError
      (Bindlib.box (Pos.same_pos_as (A.ERaise A.NoValueProvided) e))
      (Pos.get_position e)
  | D.EApp (e1, args) ->
    A.eapp (translate_expr ctx e1)
      (List.map (translate_expr ctx) args)
      (Pos.get_position e)
  | D.EAbs ((binder, pos_binder), ts) ->
    let vars, body = Bindlib.unmbind binder in
    let ctx, lc_vars =
      Array.fold_right
        (fun var (ctx, lc_vars) ->
          let lc_var = A.Var.make (Bindlib.name_of var, pos_binder) in
          D.VarMap.add var lc_var ctx, lc_var :: lc_vars)
        vars (ctx, [])
    in
    let lc_vars = Array.of_list lc_vars in
    let new_body = translate_expr ctx body in
    let new_binder = Bindlib.bind_mvar lc_vars new_body in
    Bindlib.box_apply
      (fun new_binder ->
        Pos.same_pos_as (A.EAbs ((new_binder, pos_binder), ts)) e)
      new_binder
  | D.EDefault ([exn], just, cons) when !Cli.optimize_flag ->
    A.ecatch (translate_expr ctx exn) A.EmptyError
      (A.eifthenelse (translate_expr ctx just) (translate_expr ctx cons)
         (Bindlib.box (Pos.same_pos_as (A.ERaise A.EmptyError) e))
         (Pos.get_position e))
      (Pos.get_position e)
  | D.EDefault (exceptions, just, cons) ->
    translate_default ctx exceptions just cons (Pos.get_position e)

let rec translate_scope_lets
    (decl_ctx : D.decl_ctx)
    (ctx : A.Var.t D.VarMap.t)
    (scope_lets : D.expr D.scope_body_expr) :
    A.expr D.scope_body_expr Bindlib.box =
  match scope_lets with
  | Result e -> Bindlib.box_apply (fun e -> D.Result e) (translate_expr ctx e)
  | ScopeLet scope_let ->
    let old_scope_let_var, scope_let_next =
      Bindlib.unbind scope_let.scope_let_next
    in
    let new_scope_let_var =
      A.Var.make (Bindlib.name_of old_scope_let_var, scope_let.scope_let_pos)
    in
    let new_scope_let_expr = translate_expr ctx scope_let.scope_let_expr in
    let new_ctx = D.VarMap.add old_scope_let_var new_scope_let_var ctx in
    let new_scope_next = translate_scope_lets decl_ctx new_ctx scope_let_next in
    let new_scope_next = Bindlib.bind_var new_scope_let_var new_scope_next in
    Bindlib.box_apply2
      (fun new_scope_next new_scope_let_expr ->
        D.ScopeLet
          {
            scope_let_typ = scope_let.D.scope_let_typ;
            scope_let_kind = scope_let.D.scope_let_kind;
            scope_let_pos = scope_let.D.scope_let_pos;
            scope_let_next = new_scope_next;
            scope_let_expr = new_scope_let_expr;
          })
      new_scope_next new_scope_let_expr

let rec translate_scopes
    (decl_ctx : D.decl_ctx)
    (ctx : A.Var.t D.VarMap.t)
    (scopes : D.expr D.scopes) : A.expr D.scopes Bindlib.box =
  match scopes with
  | Nil -> Bindlib.box D.Nil
  | ScopeDef scope_def ->
    let old_scope_var, scope_next = Bindlib.unbind scope_def.scope_next in
    let new_scope_var =
      A.Var.make (D.ScopeName.get_info scope_def.scope_name)
    in
    let old_scope_input_var, scope_body_expr =
      Bindlib.unbind scope_def.scope_body.scope_body_expr
    in
    let new_scope_input_var =
      A.Var.make
        ( Bindlib.name_of old_scope_input_var,
          Pos.get_position (D.ScopeName.get_info scope_def.scope_name) )
    in
    let new_ctx = D.VarMap.add old_scope_input_var new_scope_input_var ctx in
    let new_scope_body_expr =
      translate_scope_lets decl_ctx new_ctx scope_body_expr
    in
    let new_scope_body_expr =
      Bindlib.bind_var new_scope_input_var new_scope_body_expr
    in
    let new_scope : A.expr D.scope_body Bindlib.box =
      Bindlib.box_apply
        (fun new_scope_body_expr ->
          {
            D.scope_body_input_struct =
              scope_def.scope_body.scope_body_input_struct;
            scope_body_output_struct =
              scope_def.scope_body.scope_body_output_struct;
            scope_body_expr = new_scope_body_expr;
          })
        new_scope_body_expr
    in
    let new_ctx = D.VarMap.add old_scope_var new_scope_var new_ctx in
    let scope_next =
      Bindlib.bind_var new_scope_var
        (translate_scopes decl_ctx new_ctx scope_next)
    in
    Bindlib.box_apply2
      (fun new_scope scope_next ->
        D.ScopeDef
          {
            scope_name = scope_def.scope_name;
            scope_body = new_scope;
            scope_next;
          })
      new_scope scope_next

let translate_program (prgm : D.program) : A.program =
  {
    scopes =
      Bindlib.unbox (translate_scopes prgm.decl_ctx D.VarMap.empty prgm.scopes);
    decl_ctx = prgm.decl_ctx;
  }
