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

type ctx = Ast.location list

type scope_ctx = Dcalc.Ast.Var.t Ast.ScopeMap.t

let hole_var : Dcalc.Ast.Var.t = Bindlib.new_var (fun x -> (Dcalc.Ast.EVar x, Pos.no_pos)) "hole"

let translate_rules (_p : scope_ctx) (_ctx : ctx) (_rules : Ast.rule list) : Dcalc.Ast.expr * ctx =
  assert false

let translate_scope_decl (p : scope_ctx) (sigma : Ast.scope_decl) : Dcalc.Ast.expr =
  let ctx = [] in
  let _defs, ctx = translate_rules p ctx sigma.scope_decl_rules in
  let _scope_variables =
    List.filter_map (fun l -> match l with Ast.ScopeVar v -> Some v | _ -> None) ctx
  in
  assert false
