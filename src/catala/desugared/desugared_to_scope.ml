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

let translate_def (_def : Ast.rule Ast.RuleMap.t) : Scopelang.Ast.expr Pos.marked =
  (* Here, we have to transform this list of rules into a default tree. *)
  (* TODO *)
  assert false

let translate_scope (scope : Ast.scope) : Scopelang.Ast.scope_decl =
  let scope_dependencies = Dependency.build_scope_dependencies scope in
  Dependency.check_for_cycle scope scope_dependencies;
  let scope_ordering = Dependency.correct_computation_ordering scope_dependencies in
  let scope_decl_rules =
    List.flatten
      (List.map
         (fun vertex ->
           match vertex with
           | Dependency.Vertex.Var (var : Scopelang.Ast.ScopeVar.t) ->
               let var_def, var_typ =
                 Ast.ScopeDefMap.find (Ast.ScopeDef.Var var) scope.scope_defs
               in
               let expr_def = translate_def var_def in
               [
                 Scopelang.Ast.Definition
                   ( Scopelang.Ast.ScopeVar
                       (var, Pos.get_position (Scopelang.Ast.ScopeVar.get_info var)),
                     var_typ,
                     expr_def );
               ]
           | Dependency.Vertex.SubScope sub_scope_index ->
               (* Before calling the sub_scope, we need to include all the re-definitions of
                  subscope parameters*)
               let sub_scope =
                 Scopelang.Ast.SubScopeMap.find sub_scope_index scope.scope_sub_scopes
               in
               let sub_scope_vars_redefs =
                 Ast.ScopeDefMap.mapi
                   (fun def_key (def, def_typ) ->
                     match def_key with
                     | Ast.ScopeDef.Var _ -> assert false (* should not happen *)
                     | Ast.ScopeDef.SubScopeVar (_, sub_scope_var) ->
                         let expr_def = translate_def def in
                         let subscop_real_name =
                           Scopelang.Ast.SubScopeMap.find sub_scope_index scope.scope_sub_scopes
                         in
                         let var_pos =
                           Pos.get_position (Scopelang.Ast.ScopeVar.get_info sub_scope_var)
                         in
                         Scopelang.Ast.Definition
                           ( Scopelang.Ast.SubScopeVar
                               ( subscop_real_name,
                                 (sub_scope_index, var_pos),
                                 (sub_scope_var, var_pos) ),
                             def_typ,
                             expr_def ))
                   (Ast.ScopeDefMap.filter
                      (fun def_key _def ->
                        match def_key with
                        | Ast.ScopeDef.Var _ -> false
                        | Ast.ScopeDef.SubScopeVar (sub_scope_index', _) ->
                            sub_scope_index = sub_scope_index')
                      scope.scope_defs)
               in
               let sub_scope_vars_redefs =
                 List.map snd (Ast.ScopeDefMap.bindings sub_scope_vars_redefs)
               in
               sub_scope_vars_redefs @ [ Scopelang.Ast.Call (sub_scope, sub_scope_index) ])
         scope_ordering)
  in
  { Scopelang.Ast.scope_decl_name = scope.scope_uid; Scopelang.Ast.scope_decl_rules }

let translate_program (pgrm : Ast.program) : Scopelang.Ast.program =
  Scopelang.Ast.ScopeMap.map translate_scope pgrm
