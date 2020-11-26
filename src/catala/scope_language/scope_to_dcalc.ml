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
module Cli = Utils.Cli

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

let hole_var : Dcalc.Ast.Var.t = Dcalc.Ast.Var.make ("·", Pos.no_pos)

let merge_operator_var : Dcalc.Ast.Var.t = Dcalc.Ast.Var.make ("(++)", Pos.no_pos)

let merge_operator_expr : Dcalc.Ast.expr Pos.marked Bindlib.box = Bindlib.box_var merge_operator_var

let hole_expr : Dcalc.Ast.expr Pos.marked Bindlib.box = Bindlib.box_var hole_var

let rec translate_expr (ctx : ctx) (e : Ast.expr Pos.marked) : Dcalc.Ast.expr Pos.marked Bindlib.box
    =
  Bindlib.box_apply
    (fun (x : Dcalc.Ast.expr) -> Pos.same_pos_as x e)
    ( match Pos.unmark e with
    | EVar v ->
        Bindlib.box_apply
          (fun e -> Pos.unmark e)
          (Bindlib.box_var (Ast.VarMap.find v ctx.local_vars))
    | ELit l -> Bindlib.box (Dcalc.Ast.ELit l)
    | EApp (e1, args) ->
        Bindlib.box_apply2
          (fun e u -> Dcalc.Ast.EApp (e, u))
          (translate_expr ctx e1)
          (Bindlib.box_list (List.map (translate_expr ctx) args))
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
        let binder = Bindlib.bind_mvar new_xs body in
        Bindlib.box_apply (fun b -> Dcalc.Ast.EAbs (pos_binder, b, typ)) binder
    | EDefault (just, cons, subs) ->
        Bindlib.box_apply3
          (fun j c s -> Dcalc.Ast.EDefault (j, c, s))
          (translate_expr ctx just) (translate_expr ctx cons)
          (Bindlib.box_list (List.map (translate_expr ctx) subs))
    | ELocation (ScopeVar a) ->
        Bindlib.box (Dcalc.Ast.EVar (fst (Ast.ScopeVarMap.find (Pos.unmark a) ctx.scope_vars)))
    | ELocation (SubScopeVar (_, s, a)) ->
        Bindlib.box
          (Dcalc.Ast.EVar
             (fst
                (Ast.ScopeVarMap.find (Pos.unmark a)
                   (Ast.SubScopeMap.find (Pos.unmark s) ctx.subscope_vars))))
    | EIfThenElse (cond, et, ef) ->
        Bindlib.box_apply3
          (fun c t f -> Dcalc.Ast.EIfThenElse (c, t, f))
          (translate_expr ctx cond) (translate_expr ctx et) (translate_expr ctx ef)
    | EOp op -> Bindlib.box (Dcalc.Ast.EOp op) )

let translate_rule (_p : scope_ctx) (ctx : ctx) (rule : Ast.rule) :
    Dcalc.Ast.expr Pos.marked Bindlib.box * ctx =
  match rule with
  | Definition ((ScopeVar a, var_def_pos), tau, e) ->
      let a_name = Ast.ScopeVar.get_info (Pos.unmark a) in
      let a_var = Dcalc.Ast.Var.make a_name in
      let silent1 = Dcalc.Ast.Var.make ("_", Pos.get_position e) in
      let silent2 = Dcalc.Ast.Var.make ("_", Pos.get_position e) in
      let wrapped_e =
        Dcalc.Ast.make_abs
          (Array.of_list [ silent1 ])
          (translate_expr ctx e) (Pos.get_position e)
          [ (Dcalc.Ast.TUnit, var_def_pos) ]
          var_def_pos
      in
      let a_expr = Dcalc.Ast.make_var a_var in
      let merged_expr = Dcalc.Ast.make_app merge_operator_expr [ a_expr ] (Pos.get_position e) in
      let merged_expr = Dcalc.Ast.make_app merged_expr [ wrapped_e ] (Pos.get_position e) in
      let merged_thunked =
        Dcalc.Ast.make_abs
          (Array.of_list [ silent2 ])
          merged_expr (Pos.get_position e)
          [ (Dcalc.Ast.TUnit, var_def_pos) ]
          var_def_pos
      in
      let apply_thunked =
        Bindlib.box_apply2
          (fun e u -> (Dcalc.Ast.EApp (e, u), var_def_pos))
          (Bindlib.box_var a_var)
          (Bindlib.box_list [ Bindlib.box (Dcalc.Ast.ELit LUnit, var_def_pos) ])
      in
      let next_e =
        Dcalc.Ast.make_let_in (a_var, var_def_pos) tau apply_thunked hole_expr (Pos.get_position a)
      in
      let intermediate_e =
        Dcalc.Ast.make_abs
          (Array.of_list [ a_var ])
          next_e (Pos.get_position a)
          [ (Dcalc.Ast.TArrow ((TUnit, var_def_pos), tau), var_def_pos) ]
          (Pos.get_position e)
      in
      let out_e = Dcalc.Ast.make_app intermediate_e [ merged_thunked ] (Pos.get_position e) in
      let new_ctx =
        {
          ctx with
          scope_vars = Ast.ScopeVarMap.add (Pos.unmark a) (a_var, Pos.unmark tau) ctx.scope_vars;
        }
      in
      (out_e, new_ctx)
  | Definition ((SubScopeVar _, _), _tau, _e) ->
      Errors.raise_error "translation of subscope vars definitions unimplemented"
  | Call _ -> Errors.raise_error "translation of subscope calls unimplemented"

let translate_rules (p : scope_ctx) (ctx : ctx) (rules : Ast.rule list) :
    Dcalc.Ast.expr Pos.marked Bindlib.box * ctx =
  let acc = hole_expr in
  let out =
    List.fold_left
      (fun (acc, ctx) rule ->
        let new_e, ctx = translate_rule p ctx rule in
        Cli.debug_print
          (Format.asprintf "Translating rule:@\n%a" Dcalc.Print.format_expr (Bindlib.unbox new_e));
        let acc = Bindlib.bind_var hole_var acc in
        let new_acc = Bindlib.box_apply2 (fun acc new_e -> Bindlib.subst acc new_e) acc new_e in
        (new_acc, ctx))
      (acc, ctx) rules
  in
  out

let translate_scope_decl (p : scope_ctx) (sigma : Ast.scope_decl) :
    Dcalc.Ast.expr Pos.marked Bindlib.box =
  let ctx = empty_ctx in
  let rules, ctx = translate_rules p ctx sigma.scope_decl_rules in
  let scope_variables = Ast.ScopeVarMap.bindings ctx.scope_vars in
  let pos_sigma = Pos.get_position (Ast.ScopeName.get_info sigma.scope_decl_name) in
  let return_exp =
    Bindlib.box_apply
      (fun args -> (Dcalc.Ast.ETuple args, pos_sigma))
      (Bindlib.box_list
         (List.map (fun (_, (dcalc_var, _)) -> Bindlib.box_var dcalc_var) scope_variables))
  in
  let func_acc = rules in
  let func_acc =
    Dcalc.Ast.make_abs
      (Array.of_list ((List.map (fun (_, (x, _)) -> x)) scope_variables))
      func_acc pos_sigma
      (List.map
         (fun (_, (_, tau)) ->
           (Dcalc.Ast.TArrow ((Dcalc.Ast.TUnit, pos_sigma), (tau, pos_sigma)), pos_sigma))
         scope_variables)
      pos_sigma
  in
  let func_acc = Bindlib.bind_var hole_var func_acc in
  Bindlib.box_apply2
    (fun func_acc return_exp -> Bindlib.subst func_acc return_exp)
    func_acc return_exp

let translate_program (prgm : Ast.program) (top_level_scope_name : Ast.ScopeName.t) :
    Dcalc.Ast.expr Pos.marked =
  let scope_ctx =
    Ast.ScopeMap.map
      (fun scope -> Dcalc.Ast.Var.make (Ast.ScopeName.get_info scope.Ast.scope_decl_name))
      prgm
  in
  let scope_dependencies = Dependency.build_program_dep_graph prgm in
  Dependency.check_for_cycle scope_dependencies;
  let scope_ordering = Dependency.get_scope_ordering scope_dependencies in
  let top_level_scope = Ast.ScopeMap.find top_level_scope_name prgm in
  let acc = translate_scope_decl scope_ctx top_level_scope in
  (* the resulting expression is the list of definitions of all the scopes, ending with the
     top-level scope. *)
  Bindlib.unbox
    (List.fold_right
       (fun scope_name (acc : Dcalc.Ast.expr Pos.marked Bindlib.box) ->
         if scope_name = top_level_scope_name then acc
         else
           let scope = Ast.ScopeMap.find scope_name prgm in
           let scope_expr = translate_scope_decl scope_ctx scope in
           (* here we perform type-checking, incidentally *)
           let scope_typ = Dcalc.Typing.infer_type (Bindlib.unbox scope_expr) in
           Dcalc.Ast.make_let_in
             (Ast.ScopeMap.find scope_name scope_ctx, Pos.get_position scope_typ)
             scope_typ scope_expr acc Pos.no_pos)
       scope_ordering acc)
