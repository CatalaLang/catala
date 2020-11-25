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

module Pos = Utils.Pos
module Errors = Utils.Errors

type ctx = {
  scope_vars : (Dcalc.Ast.Var.t * Dcalc.Ast.typ) Ast.ScopeVarMap.t;
  subscope_vars : (Dcalc.Ast.Var.t * Dcalc.Ast.typ) Ast.ScopeVarMap.t Ast.SubScopeMap.t;
  local_vars : Dcalc.Ast.Var.t Ast.VarMap.t;
}

let empty_ctx =
  {
    scope_vars = Ast.ScopeVarMap.empty;
    subscope_vars = Ast.SubScopeMap.empty;
    local_vars = Ast.VarMap.empty;
  }

type scope_ctx = Dcalc.Ast.Var.t Ast.ScopeMap.t

let hole_var : Dcalc.Ast.Var.t = Dcalc.Ast.Var.make ("hole", Pos.no_pos)

let merge_operator_var : Dcalc.Ast.Var.t = Dcalc.Ast.Var.make ("merge", Pos.no_pos)

let merge_operator_expr : Dcalc.Ast.expr Pos.marked Bindlib.box = Bindlib.box_var merge_operator_var

let hole_expr : Dcalc.Ast.expr Pos.marked Bindlib.box = Bindlib.box_var hole_var

let rec translate_expr (ctx : ctx) (e : Ast.expr Pos.marked) : Dcalc.Ast.expr Pos.marked =
  Pos.same_pos_as
    ( match Pos.unmark e with
    | EVar v -> Dcalc.Ast.EVar (Ast.VarMap.find v ctx.local_vars)
    | ELit l -> Dcalc.Ast.ELit l
    | EApp (e1, args) -> Dcalc.Ast.EApp (translate_expr ctx e1, List.map (translate_expr ctx) args)
    | EAbs (pos_binder, binder, typ) ->
        let xs, body = Bindlib.unmbind binder in
        let new_xs = Array.map (fun x -> Dcalc.Ast.Var.make (Bindlib.name_of x, pos_binder)) xs in
        let both_xs = Array.map2 (fun x new_x -> (x, new_x)) xs new_xs in
        let body =
          translate_expr
            {
              ctx with
              local_vars =
                Array.fold_left
                  (fun local_vars (x, new_x) -> Ast.VarMap.add x new_x local_vars)
                  ctx.local_vars both_xs;
            }
            body
        in
        let binder = Bindlib.unbox (Bindlib.bind_mvar new_xs (Bindlib.box body)) in
        Dcalc.Ast.EAbs (pos_binder, binder, typ)
    | EDefault (just, cons, subs) ->
        Dcalc.Ast.EDefault
          (translate_expr ctx just, translate_expr ctx cons, List.map (translate_expr ctx) subs)
    | ELocation (ScopeVar a) ->
        Dcalc.Ast.EVar (fst (Ast.ScopeVarMap.find (Pos.unmark a) ctx.scope_vars))
    | ELocation (SubScopeVar (_, s, a)) ->
        Dcalc.Ast.EVar
          (fst
             (Ast.ScopeVarMap.find (Pos.unmark a)
                (Ast.SubScopeMap.find (Pos.unmark s) ctx.subscope_vars)))
    | EIfThenElse (cond, et, ef) ->
        Dcalc.Ast.EIfThenElse (translate_expr ctx cond, translate_expr ctx et, translate_expr ctx ef)
    | EOp op -> EOp op )
    e

let translate_rule (_p : scope_ctx) (ctx : ctx) (rule : Ast.rule) :
    Dcalc.Ast.expr Pos.marked Bindlib.box * ctx =
  match rule with
  | Definition (ScopeVar a, tau, e) ->
      let a_name = Ast.ScopeVar.get_info (Pos.unmark a) in
      let a_var = Dcalc.Ast.Var.make a_name in
      let next_e =
        Dcalc.Ast.make_abs
          (Array.of_list [ a_var ])
          hole_expr (Pos.get_position a) [ tau ] (Pos.get_position e)
      in
      let silent1 = Dcalc.Ast.Var.make ("silent", Pos.get_position e) in
      let silent2 = Dcalc.Ast.Var.make ("silent", Pos.get_position e) in
      let wrapped_e =
        Dcalc.Ast.make_abs
          (Array.of_list [ silent1 ])
          (Bindlib.box (translate_expr ctx e))
          (Pos.get_position e) [ Dcalc.Ast.TUnit ] (Pos.get_position e)
      in
      let a_expr = Dcalc.Ast.make_var a_var in
      let merged_expr = Dcalc.Ast.make_app merge_operator_expr [ a_expr ] (Pos.get_position e) in
      let merged_expr = Dcalc.Ast.make_app merged_expr [ wrapped_e ] (Pos.get_position e) in
      let merged_thunked =
        Dcalc.Ast.make_abs
          (Array.of_list [ silent2 ])
          merged_expr (Pos.get_position e) [ Dcalc.Ast.TUnit ] (Pos.get_position e)
      in
      let final_e = Dcalc.Ast.make_app merged_thunked [ next_e ] (Pos.get_position e) in
      let new_ctx =
        { ctx with scope_vars = Ast.ScopeVarMap.add (Pos.unmark a) (a_var, tau) ctx.scope_vars }
      in
      (final_e, new_ctx)
  | Definition (SubScopeVar _, _tau, _e) ->
      Errors.raise_error "translation of subscope vars definitions unimplemented"
  | Call _ -> Errors.raise_error "translation of subscope calls unimplemented"

let translate_rules (p : scope_ctx) (ctx : ctx) (rules : Ast.rule list) :
    Dcalc.Ast.expr Pos.marked Bindlib.box * ctx =
  let acc = hole_expr in
  List.fold_left
    (fun (acc, ctx) rule ->
      let new_e, ctx = translate_rule p ctx rule in
      let acc = Bindlib.unbox (Bindlib.bind_var hole_var acc) in
      let new_acc = Bindlib.subst acc (Bindlib.unbox new_e) in
      (Bindlib.box new_acc, ctx))
    (acc, ctx) rules

let translate_scope_decl (p : scope_ctx) (sigma : Ast.scope_decl) : Dcalc.Ast.expr Pos.marked =
  let ctx = empty_ctx in
  let rules, ctx = translate_rules p ctx sigma.scope_decl_rules in
  let scope_variables = Ast.ScopeVarMap.bindings ctx.scope_vars in
  let pos_sigma = Pos.get_position (Ast.ScopeName.get_info sigma.scope_decl_name) in
  let return_exp =
    Dcalc.Ast.ETuple
      (List.map (fun (_, (dcalc_var, _)) -> (Dcalc.Ast.EVar dcalc_var, pos_sigma)) scope_variables)
  in
  let func_acc = rules in
  let func_acc =
    Dcalc.Ast.make_abs
      (Array.of_list ((List.map (fun (_, (x, _)) -> x)) scope_variables))
      func_acc pos_sigma
      (List.map
         (fun (_, (_, tau)) -> Dcalc.Ast.TArrow ((Dcalc.Ast.TUnit, pos_sigma), (tau, pos_sigma)))
         scope_variables)
      pos_sigma
  in
  let func_acc = Bindlib.unbox (Bindlib.bind_var hole_var func_acc) in
  Bindlib.subst func_acc (return_exp, pos_sigma)

let translate_program (prgm : Ast.program) (_top_level_scope : Ast.ScopeName.t) :
    Dcalc.Ast.expr Pos.marked =
  let _scope_ctx =
    Ast.ScopeMap.map
      (fun scope -> Ast.Var.make (Ast.ScopeName.get_info scope.Ast.scope_decl_name))
      prgm
  in
  (* TODO: compute dependency order! *)
  assert false
