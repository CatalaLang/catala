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
  var_dict : A.LocalVarName.t L.VarMap.t;
  inside_definition_of : A.LocalVarName.t list;
}

let translate_expr (_ctxt : ctxt) (_expr : L.expr Pos.marked) : A.expr Pos.marked = assert false

let rec translate_block (ctxt : ctxt) (block_expr : L.expr Pos.marked) : A.block Pos.marked =
  match Pos.unmark block_expr with
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
          (fun (x, _) arg -> ((L.VarMap.find x ctxt.var_dict, binder_pos), arg))
          vars_tau args
      in
      let def_blocks =
        List.map
          (fun (x, arg) ->
            let ctxt =
              { ctxt with inside_definition_of = Pos.unmark x :: ctxt.inside_definition_of }
            in
            translate_block ctxt arg)
          vars_args
      in
      let rest_of_block = translate_block ctxt body in
      ( local_decls
        @ List.map (fun def -> (A.SBlock (Pos.unmark def), Pos.get_position def)) def_blocks
        @ Pos.unmark rest_of_block,
        Pos.get_position rest_of_block )
  | _ -> (
      (* We have reached the bottom of the L.expr, which means that we have to return it. If we are
         at the top-level block of the function, we insert a return statement. But if this block was
         used inside the definition of a local var, we define this local var (which should be on top
         of the stack) *)
      match ctxt.inside_definition_of with
      | hd :: _ ->
          ( [
              ( A.SLocalDef (Pos.same_pos_as hd block_expr, translate_expr ctxt block_expr),
                Pos.get_position block_expr );
            ],
            Pos.get_position block_expr )
      | [] ->
          ( [
              (A.SReturn (Pos.unmark (translate_expr ctxt block_expr)), Pos.get_position block_expr);
            ],
            Pos.get_position block_expr ))

let translate_scope (func_dict : A.FuncName.t L.VarMap.t) (scope_expr : L.expr Pos.marked) :
    (A.LocalVarName.t Pos.marked * D.typ Pos.marked) list * A.block Pos.marked =
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
      let new_body = translate_block { func_dict; var_dict; inside_definition_of = [] } body in
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
             let new_scope_params, new_scope_body = translate_scope func_dict scope_expr in
             let func_id = A.FuncName.fresh (Bindlib.name_of scope_name, Pos.no_pos) in
             let func_dict = L.VarMap.add scope_name func_id func_dict in
             (func_dict, (func_id, new_scope_params, new_scope_body) :: new_scopes))
           (L.VarMap.empty, []) p.L.scopes
       in
       List.rev new_scopes);
  }
