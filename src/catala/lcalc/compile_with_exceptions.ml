(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils
module D = Dcalc.Ast
module A = Ast

type ctx = A.expr Pos.marked Bindlib.box D.VarMap.t

let handle_default pos = A.make_var (A.Var.make ("handle_default", pos), pos)

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

let thunk_expr (e : A.expr Pos.marked Bindlib.box) (pos : Pos.t) : A.expr Pos.marked Bindlib.box =
  let dummy_var = A.Var.make ("_", pos) in
  A.make_abs [| dummy_var |] e pos [ (D.TAny, pos) ] pos

let rec translate_default (ctx : ctx) (exceptions : D.expr Pos.marked list)
    (just : D.expr Pos.marked) (cons : D.expr Pos.marked) (pos_default : Pos.t) :
    A.expr Pos.marked Bindlib.box =
  let exceptions =
    List.map (fun except -> thunk_expr (translate_expr ctx except) pos_default) exceptions
  in
  let exceptions =
    A.make_app (handle_default pos_default)
      [
        Bindlib.box_apply
          (fun exceptions -> (A.EArray exceptions, pos_default))
          (Bindlib.box_list exceptions);
        thunk_expr (translate_expr ctx just) pos_default;
        thunk_expr (translate_expr ctx cons) pos_default;
      ]
      pos_default
  in
  exceptions

and translate_expr (ctx : ctx) (e : D.expr Pos.marked) : A.expr Pos.marked Bindlib.box =
  match Pos.unmark e with
  | D.EVar v -> D.VarMap.find (Pos.unmark v) ctx
  | D.ETuple (args, s) ->
      Bindlib.box_apply
        (fun args -> Pos.same_pos_as (A.ETuple (args, s)) e)
        (Bindlib.box_list (List.map (translate_expr ctx) args))
  | D.ETupleAccess (e1, i, s, ts) ->
      Bindlib.box_apply
        (fun e1 -> Pos.same_pos_as (A.ETupleAccess (e1, i, s, ts)) e)
        (translate_expr ctx e1)
  | D.EInj (e1, i, en, ts) ->
      Bindlib.box_apply
        (fun e1 -> Pos.same_pos_as (A.EInj (e1, i, en, ts)) e)
        (translate_expr ctx e1)
  | D.EMatch (e1, cases, en) ->
      Bindlib.box_apply2
        (fun e1 cases -> Pos.same_pos_as (A.EMatch (e1, cases, en)) e)
        (translate_expr ctx e1)
        (Bindlib.box_list (List.map (translate_expr ctx) cases))
  | D.EArray es ->
      Bindlib.box_apply
        (fun es -> Pos.same_pos_as (A.EArray es) e)
        (Bindlib.box_list (List.map (translate_expr ctx) es))
  | D.ELit l -> Bindlib.box (Pos.same_pos_as (translate_lit l) e)
  | D.EOp op -> Bindlib.box (Pos.same_pos_as (A.EOp op) e)
  | D.EIfThenElse (e1, e2, e3) ->
      Bindlib.box_apply3
        (fun e1 e2 e3 -> Pos.same_pos_as (A.EIfThenElse (e1, e2, e3)) e)
        (translate_expr ctx e1) (translate_expr ctx e2) (translate_expr ctx e3)
  | D.EAssert e1 ->
      Bindlib.box_apply (fun e1 -> Pos.same_pos_as (A.EAssert e1) e) (translate_expr ctx e1)
  | D.EApp (e1, args) ->
      Bindlib.box_apply2
        (fun e1 args -> Pos.same_pos_as (A.EApp (e1, args)) e)
        (translate_expr ctx e1)
        (Bindlib.box_list (List.map (translate_expr ctx) args))
  | D.EAbs ((binder, pos_binder), ts) ->
      let vars, body = Bindlib.unmbind binder in
      let ctx, lc_vars =
        Array.fold_right
          (fun var (ctx, lc_vars) ->
            let lc_var = A.Var.make (Bindlib.name_of var, pos_binder) in
            let lc_var_expr = A.make_var (lc_var, pos_binder) in
            (D.VarMap.add var lc_var_expr ctx, lc_var :: lc_vars))
          vars (ctx, [])
      in
      let lc_vars = Array.of_list lc_vars in
      let new_body = translate_expr ctx body in
      let new_binder = Bindlib.bind_mvar lc_vars new_body in
      Bindlib.box_apply
        (fun new_binder -> Pos.same_pos_as (A.EAbs ((new_binder, pos_binder), ts)) e)
        new_binder
  | D.EDefault (exceptions, just, cons) ->
      translate_default ctx exceptions just cons (Pos.get_position e)

let translate_program (prgm : D.program) : A.program =
  {
    scopes =
      (let acc, _ =
         List.fold_left
           (fun ((acc, ctx) : 'a * A.Var.t D.VarMap.t) (_, n, e) ->
             let new_n = A.Var.make (Bindlib.name_of n, Pos.no_pos) in
             let new_acc =
               ( new_n,
                 Bindlib.unbox
                   (translate_expr (D.VarMap.map (fun v -> A.make_var (v, Pos.no_pos)) ctx) e) )
               :: acc
             in
             let new_ctx = D.VarMap.add n new_n ctx in
             (new_acc, new_ctx))
           ([], D.VarMap.empty) prgm.scopes
       in
       List.rev acc);
    decl_ctx = prgm.decl_ctx;
  }
