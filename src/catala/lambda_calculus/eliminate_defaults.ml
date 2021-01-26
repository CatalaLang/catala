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

let translate_lit (l : D.lit) (pos_error : Pos.t) : A.lit =
  match l with
  | D.LBool l -> A.LBool l
  | D.LInt i -> A.LInt i
  | D.LRat r -> A.LRat r
  | D.LMoney m -> A.LMoney m
  | D.LUnit -> A.LUnit
  | D.LDate d -> A.LDate d
  | D.LDuration d -> A.LDuration d
  | D.LEmptyError ->
      Errors.raise_spanned_error
        "Impossible to translate from default calculus to lambdacalculus: toplevel empty error \
         term not under a default"
        pos_error

let just_id_counter = ref 0

(** [eliminate_default_compile_scheme ctx exceptions just cons] returns a tuple,
    [just_vars_list, compiled_expr] where just_var_list is the bundle of all the let bindings for
    justifications in the default, and compiled expr represents the output of the compile scheme.
    [compiled_expr] is a function because it's actually a list of nested conditional where the last
    else branch is empty *)
let rec eliminate_default_compile_scheme (ctx : ctx) (exceptions : D.expr Pos.marked list)
    (just : D.expr Pos.marked) (cons : D.expr Pos.marked) (pos_default : Pos.t) :
    (A.Var.t * A.expr Pos.marked Bindlib.box) list
    * (A.expr Pos.marked Bindlib.box -> A.expr Pos.marked Bindlib.box) =
  let just_vars_exceptions_lists, compiled_exceptions_exprs =
    List.split
      (List.map
         (fun except ->
           match Pos.unmark except with
           | D.EDefault (except_exceptions, except_just, except_cons) ->
               eliminate_default_compile_scheme ctx except_exceptions except_just except_cons
                 (Pos.get_position except)
           | _ ->
               Errors.raise_spanned_error
                 "Impossible to translate from default calculus to lambda calculus: exception is \
                  not a defautlt"
                 (Pos.get_position except))
         exceptions)
  in
  let just_expr = eliminate_defaults_expr ctx just in
  let just_var = A.Var.make (Pos.same_pos_as (Format.asprintf "just_%d" !just_id_counter) just) in
  incr just_id_counter;
  let just_vars_exceptions_list = List.flatten just_vars_exceptions_lists in

  let compiled_exceptions_expr : A.expr Pos.marked Bindlib.box -> A.expr Pos.marked Bindlib.box =
    List.fold_left
      (fun acc exception_expr next -> acc (exception_expr next))
      (fun x -> x)
      compiled_exceptions_exprs
  in
  let just_var_list = (just_var, just_expr) :: just_vars_exceptions_list in
  ( just_var_list,
    fun next ->
      compiled_exceptions_expr
        (Bindlib.box_apply3
           (fun just_var_expr cons_expr next ->
             (A.EIfThenElse (just_var_expr, cons_expr, next), pos_default))
           (A.make_var (just_var, pos_default))
           (eliminate_defaults_expr ctx cons)
           next) )

and eliminate_default (ctx : ctx) (exceptions : D.expr Pos.marked list) (just : D.expr Pos.marked)
    (cons : D.expr Pos.marked) (pos_default : Pos.t) : A.expr Pos.marked Bindlib.box =
  let _, compiled_expr = eliminate_default_compile_scheme ctx exceptions just cons pos_default in
  compiled_expr (Bindlib.box (A.ELit A.LCrash, pos_default))

and eliminate_defaults_expr (ctx : ctx) (e : D.expr Pos.marked) : A.expr Pos.marked Bindlib.box =
  match Pos.unmark e with
  | D.EVar v -> D.VarMap.find (Pos.unmark v) ctx
  | D.ETuple (args, s) ->
      Bindlib.box_apply
        (fun args -> Pos.same_pos_as (A.ETuple (args, s)) e)
        (Bindlib.box_list (List.map (eliminate_defaults_expr ctx) args))
  | D.ETupleAccess (e1, i, s, ts) ->
      Bindlib.box_apply
        (fun e1 -> Pos.same_pos_as (A.ETupleAccess (e1, i, s, ts)) e)
        (eliminate_defaults_expr ctx e1)
  | D.EInj (e1, i, en, ts) ->
      Bindlib.box_apply
        (fun e1 -> Pos.same_pos_as (A.EInj (e1, i, en, ts)) e)
        (eliminate_defaults_expr ctx e1)
  | D.EMatch (e1, cases, en) ->
      Bindlib.box_apply2
        (fun e1 cases -> Pos.same_pos_as (A.EMatch (e1, cases, en)) e)
        (eliminate_defaults_expr ctx e1)
        (Bindlib.box_list (List.map (eliminate_defaults_expr ctx) cases))
  | D.EArray es ->
      Bindlib.box_apply
        (fun es -> Pos.same_pos_as (A.EArray es) e)
        (Bindlib.box_list (List.map (eliminate_defaults_expr ctx) es))
  | D.ELit l -> Bindlib.box (Pos.same_pos_as (A.ELit (translate_lit l (Pos.get_position e))) e)
  | D.EOp op -> Bindlib.box (Pos.same_pos_as (A.EOp op) e)
  | D.EIfThenElse (e1, e2, e3) ->
      Bindlib.box_apply3
        (fun e1 e2 e3 -> Pos.same_pos_as (A.EIfThenElse (e1, e2, e3)) e)
        (eliminate_defaults_expr ctx e1) (eliminate_defaults_expr ctx e2)
        (eliminate_defaults_expr ctx e3)
  | D.EAssert e1 ->
      Bindlib.box_apply
        (fun e1 -> Pos.same_pos_as (A.EAssert e1) e)
        (eliminate_defaults_expr ctx e1)
  | D.EApp (e1, args) ->
      Bindlib.box_apply2
        (fun e1 args -> Pos.same_pos_as (A.EApp (e1, args)) e)
        (eliminate_defaults_expr ctx e1)
        (Bindlib.box_list (List.map (eliminate_defaults_expr ctx) args))
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
      let new_body = eliminate_defaults_expr ctx body in
      let new_binder = Bindlib.bind_mvar lc_vars new_body in
      Bindlib.box_apply
        (fun new_binder -> Pos.same_pos_as (A.EAbs (pos_binder, new_binder, ts)) e)
        new_binder
  | D.EDefault (exceptions, just, cons) ->
      eliminate_default ctx exceptions just cons (Pos.get_position e)

let translate_expr (e : D.expr Pos.marked) : A.expr Pos.marked =
  Bindlib.unbox (eliminate_defaults_expr D.VarMap.empty e)
