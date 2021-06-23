(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils
module A = Ast
module L = Lcalc.Ast
module D = Dcalc.Ast

type ctxt = {
  func_dict : A.FuncName.t L.VarMap.t;
  decl_ctx : D.decl_ctx;
  var_dict : A.LocalVarName.t L.VarMap.t;
  inside_definition_of : A.LocalVarName.t option;
}

(* Expressions can spill out side effect, hence this function also returns a list of statements to
   be prepended before the expression is evaluated *)
let rec translate_expr (ctxt : ctxt) (expr : L.expr Pos.marked) : A.block * A.expr Pos.marked =
  Cli.warning_print
    (Format.asprintf "Translating expression %a" (Lcalc.Print.format_expr ctxt.decl_ctx) expr);
  match Pos.unmark expr with
  | L.EVar v ->
      let local_var =
        try A.EVar (L.VarMap.find (Pos.unmark v) ctxt.var_dict)
        with Not_found -> A.EFunc (L.VarMap.find (Pos.unmark v) ctxt.func_dict)
      in
      ([], (local_var, Pos.get_position v))
  | L.ETupleAccess (e1, num_field, Some s_name, _) ->
      let e1_stmts, new_e1 = translate_expr ctxt e1 in
      let field_name =
        fst (List.nth (D.StructMap.find s_name ctxt.decl_ctx.ctx_structs) num_field)
      in
      (e1_stmts, (A.EStructFieldAccess (new_e1, field_name, s_name), Pos.get_position expr))
  | L.EApp (f, args) ->
      let f_stmts, new_f = translate_expr ctxt f in
      let args_stmts, new_args =
        List.fold_left
          (fun (args_stmts, new_args) arg ->
            let arg_stmts, new_arg = translate_expr ctxt arg in
            (arg_stmts @ args_stmts, new_arg :: new_args))
          ([], []) args
      in
      let new_args = List.rev new_args in
      let args_stmts = List.rev args_stmts in
      (f_stmts @ args_stmts, (A.EApp (new_f, new_args), Pos.get_position expr))
  | L.EArray args ->
      let args_stmts, new_args =
        List.fold_left
          (fun (args_stmts, new_args) arg ->
            let arg_stmts, new_arg = translate_expr ctxt arg in
            (arg_stmts @ args_stmts, new_arg :: new_args))
          ([], []) args
      in
      let new_args = List.rev new_args in
      let args_stmts = List.rev args_stmts in
      (args_stmts, (A.EArray new_args, Pos.get_position expr))
  | L.EOp op -> ([], (A.EOp op, Pos.get_position expr))
  | L.ELit l -> ([], (A.ELit l, Pos.get_position expr))
  | _ ->
      Cli.warning_print "It's a statement!";
      let tmp_var = A.LocalVarName.fresh ("tmp", Pos.get_position expr) in
      let ctxt = { ctxt with inside_definition_of = Some tmp_var } in
      let tmp_stmts = translate_statements ctxt expr in
      ( ( A.SLocalDecl ((tmp_var, Pos.get_position expr), (D.TAny, Pos.get_position expr)),
          Pos.get_position expr )
        :: tmp_stmts,
        (A.EVar tmp_var, Pos.get_position expr) )

and translate_statements (ctxt : ctxt) (block_expr : L.expr Pos.marked) : A.block =
  Cli.warning_print
    (Format.asprintf "Translating statement %a" (Lcalc.Print.format_expr ctxt.decl_ctx) block_expr);
  match Pos.unmark block_expr with
  | L.EApp ((L.EAbs ((binder, _), [ (D.TLit D.TUnit, _) ]), _), [ (L.EAssert e, _) ]) ->
      (* Assertions are always encapsulated in a unit-typed let binding *)
      let _, body = Bindlib.unmbind binder in
      let e_stmts, new_e = translate_expr ctxt e in
      e_stmts
      @ (A.SAssert (Pos.unmark new_e), Pos.get_position block_expr)
        :: translate_statements ctxt body
  | L.EApp ((L.EAbs ((binder, binder_pos), taus), eabs_pos), args) ->
      (* This defines multiple local variables at the time *)
      let vars, body = Bindlib.unmbind binder in
      let vars_tau = List.map2 (fun x tau -> (x, tau)) (Array.to_list vars) taus in
      let ctxt =
        {
          ctxt with
          var_dict =
            List.fold_left
              (fun var_dict (x, _) ->
                L.VarMap.add x (A.LocalVarName.fresh (Bindlib.name_of x, binder_pos)) var_dict)
              ctxt.var_dict vars_tau;
        }
      in
      let local_decls =
        List.map
          (fun (x, tau) ->
            (A.SLocalDecl ((L.VarMap.find x ctxt.var_dict, binder_pos), tau), eabs_pos))
          vars_tau
      in
      let vars_args =
        List.map2
          (fun (x, tau) arg -> ((L.VarMap.find x ctxt.var_dict, binder_pos), tau, arg))
          vars_tau args
      in
      let def_blocks =
        List.map
          (fun (x, _tau, arg) ->
            let ctxt = { ctxt with inside_definition_of = Some (Pos.unmark x) } in
            let arg_stmts, new_arg = translate_expr ctxt arg in
            arg_stmts @ [ (A.SLocalDef (x, new_arg), binder_pos) ])
          vars_args
      in
      let rest_of_block = translate_statements ctxt body in
      local_decls @ List.flatten def_blocks @ rest_of_block
  | L.EIfThenElse (cond, e_true, e_false) ->
      let cond_stmts, s_cond = translate_expr ctxt cond in
      let s_e_true = translate_statements ctxt e_true in
      let s_e_false = translate_statements ctxt e_false in
      cond_stmts @ [ (A.SIfThenElse (s_cond, s_e_true, s_e_false), Pos.get_position block_expr) ]
  | L.ECatch (e_try, except, e_catch) ->
      let s_e_try = translate_statements ctxt e_try in
      let s_e_catch = translate_statements ctxt e_catch in
      [ (A.STryExcept (s_e_try, except, s_e_catch), Pos.get_position block_expr) ]
  | L.ERaise except -> [ (A.SRaise except, Pos.get_position block_expr) ]
  | _ ->
      Cli.warning_print "Falling back to return expr!";
      let e_stmts, new_e = translate_expr ctxt block_expr in
      e_stmts @ [ (A.SReturn (Pos.unmark new_e), Pos.get_position block_expr) ]

let translate_scope (decl_ctx : D.decl_ctx) (func_dict : A.FuncName.t L.VarMap.t)
    (scope_expr : L.expr Pos.marked) :
    (A.LocalVarName.t Pos.marked * D.typ Pos.marked) list * A.block =
  match Pos.unmark scope_expr with
  | L.EAbs ((binder, binder_pos), typs) ->
      let vars, body = Bindlib.unmbind binder in
      let var_dict =
        Array.fold_left
          (fun var_dict var ->
            L.VarMap.add var (A.LocalVarName.fresh (Bindlib.name_of var, binder_pos)) var_dict)
          L.VarMap.empty vars
      in
      let param_list =
        List.map2
          (fun var typ -> ((L.VarMap.find var var_dict, binder_pos), typ))
          (Array.to_list vars) typs
      in
      let new_body =
        translate_statements { decl_ctx; func_dict; var_dict; inside_definition_of = None } body
      in
      (param_list, new_body)
  | _ -> assert false
(* should not happen *)

let translate_program (p : L.program) : A.program =
  {
    decl_ctx = p.L.decl_ctx;
    scopes =
      (let _, new_scopes =
         List.fold_left
           (fun (func_dict, new_scopes) (scope_name, scope_expr) ->
             let new_scope_params, new_scope_body =
               translate_scope p.decl_ctx func_dict scope_expr
             in
             let func_id = A.FuncName.fresh (Bindlib.name_of scope_name, Pos.no_pos) in
             let func_dict = L.VarMap.add scope_name func_id func_dict in
             (func_dict, (func_id, new_scope_params, new_scope_body) :: new_scopes))
           ( L.VarMap.singleton L.handle_default (A.FuncName.fresh ("handle_default", Pos.no_pos)),
             [] )
           p.L.scopes
       in
       List.rev new_scopes);
  }
