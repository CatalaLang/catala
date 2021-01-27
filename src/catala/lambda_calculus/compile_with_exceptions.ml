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

let option_enum = D.EnumName.fresh ("Option", Pos.no_pos)

let option_sig = [ (D.TAny, Pos.no_pos); (D.TLit D.TUnit, Pos.no_pos) ]

let option_ctx =
  List.combine
    [ D.EnumConstructor.fresh ("Some", Pos.no_pos); D.EnumConstructor.fresh ("None", Pos.no_pos) ]
    option_sig

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

let rec translate_default (ctx : ctx) (exceptions : D.expr Pos.marked list)
    (_just : D.expr Pos.marked) (_cons : D.expr Pos.marked) (pos_default : Pos.t) :
    A.expr Pos.marked Bindlib.box =
  let none_expr =
    Bindlib.box (A.EInj ((A.ELit A.LUnit, pos_default), 1, option_enum, option_sig), pos_default)
  in
  let some_expr e =
    Bindlib.box_apply (fun e -> (A.EInj (e, 0, option_enum, option_sig), pos_default)) e
  in
  let acc_var = A.Var.make ("acc", pos_default) in
  let exc_var = A.Var.make ("exc", pos_default) in
  let acc_some_var = A.Var.make ("acc", pos_default) in
  let acc_none_var = A.Var.make ("_", pos_default) in
  let exc_some_var = A.Var.make ("_", pos_default) in
  let exc_none_var = A.Var.make ("_", pos_default) in
  let exc_none_case_body = some_expr (A.make_var (acc_some_var, pos_default)) in
  let exc_some_case_body = Bindlib.box (A.ERaise A.ConflictError, pos_default) in
  let acc_some_case_body =
    Bindlib.box_apply4
      (fun some_exc_var exc_none_case exc_some_case none_expr ->
        ( A.EMatch
            ( (A.ECatch (some_exc_var, A.EmptyError, none_expr), pos_default),
              [ exc_some_case; exc_none_case ],
              option_enum ),
          pos_default ))
      (some_expr (A.make_var (exc_var, pos_default)))
      (A.make_abs [| exc_none_var |] exc_none_case_body pos_default
         [ (D.TLit D.TUnit, pos_default) ] pos_default)
      (A.make_abs [| exc_some_var |] exc_some_case_body pos_default [ (D.TAny, pos_default) ]
         pos_default)
      none_expr
  in
  let acc_none_case_body =
    Bindlib.box_apply2
      (fun some_exc_var none_expr ->
        (A.ECatch (some_exc_var, A.EmptyError, none_expr), pos_default))
      (some_expr (A.make_var (exc_var, pos_default)))
      none_expr
  in
  let fold_body =
    Bindlib.box_apply3
      (fun acc_var acc_none_case acc_some_case ->
        (A.EMatch (acc_var, [ acc_some_case; acc_none_case ], option_enum), pos_default))
      (A.make_var (acc_var, pos_default))
      (A.make_abs [| acc_none_var |] acc_none_case_body pos_default
         [ (D.TLit D.TUnit, pos_default) ] pos_default)
      (A.make_abs [| acc_some_var |] acc_some_case_body pos_default [ (D.TAny, pos_default) ]
         pos_default)
  in
  let fold_func =
    A.make_abs [| acc_var; exc_var |] fold_body pos_default
      [ (D.TAny, pos_default); (D.TAny, pos_default) ]
      pos_default
  in
  let exceptions = List.map (translate_expr ctx) exceptions in
  let exceptions =
    A.make_app
      (Bindlib.box (A.EOp (D.Ternop D.Fold), pos_default))
      [
        fold_func;
        none_expr;
        Bindlib.box_apply
          (fun exceptions -> (A.EArray exceptions, pos_default))
          (Bindlib.box_list exceptions);
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
  | D.EAbs (pos_binder, binder, ts) ->
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
        (fun new_binder -> Pos.same_pos_as (A.EAbs (pos_binder, new_binder, ts)) e)
        new_binder
  | D.EDefault (exceptions, just, cons) ->
      translate_default ctx exceptions just cons (Pos.get_position e)

let translate_expr (e : D.expr Pos.marked) (ctx : D.decl_ctx) : A.expr Pos.marked * D.decl_ctx =
  ( Bindlib.unbox (translate_expr D.VarMap.empty e),
    { ctx with D.ctx_enums = D.EnumMap.add option_enum option_ctx ctx.D.ctx_enums } )
