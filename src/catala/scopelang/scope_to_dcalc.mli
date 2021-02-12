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

type scope_sigs_ctx =
  (* list of scope variables with their types *)
  ( (Ast.ScopeVar.t * Dcalc.Ast.typ) list
  * (* var representing the scope *) Dcalc.Ast.Var.t
  * (* var representing the scope input inside the scope func *) Dcalc.Ast.Var.t
  * (* scope input *) Ast.StructName.t
  * (* scope output *) Ast.StructName.t )
  Ast.ScopeMap.t

type ctx = {
  structs : Ast.struct_ctx;
  enums : Ast.enum_ctx;
  scope_name : Ast.ScopeName.t;
  scopes_parameters : scope_sigs_ctx;
  scope_vars : (Dcalc.Ast.Var.t * Dcalc.Ast.typ) Ast.ScopeVarMap.t;
  subscope_vars : (Dcalc.Ast.Var.t * Dcalc.Ast.typ) Ast.ScopeVarMap.t Ast.SubScopeMap.t;
  local_vars : Dcalc.Ast.Var.t Ast.VarMap.t;
}

val empty_ctx : Ast.struct_ctx -> Ast.enum_ctx -> scope_sigs_ctx -> Ast.ScopeName.t -> ctx

type scope_ctx = Dcalc.Ast.Var.t Ast.ScopeMap.t

val hole_var : Dcalc.Ast.Var.t

val translate_typ : ctx -> Ast.typ Pos.marked -> Dcalc.Ast.typ Pos.marked

val merge_defaults :
  Dcalc.Ast.expr Pos.marked Bindlib.box ->
  Dcalc.Ast.expr Pos.marked Bindlib.box ->
  Dcalc.Ast.expr Pos.marked Bindlib.box

val tag_with_log_entry :
  Dcalc.Ast.expr Pos.marked Bindlib.box ->
  Dcalc.Ast.log_entry ->
  Uid.MarkedString.info list ->
  Dcalc.Ast.expr Pos.marked Bindlib.box

val translate_expr : ctx -> Ast.expr Pos.marked -> Dcalc.Ast.expr Pos.marked Bindlib.box

val translate_rule :
  ctx ->
  Ast.rule ->
  Ast.rule list ->
  Uid.MarkedString.info ->
  Ast.StructName.t ->
  Dcalc.Ast.expr Pos.marked Bindlib.box * ctx

val translate_rules :
  ctx ->
  Ast.rule list ->
  Uid.MarkedString.info ->
  Ast.StructName.t ->
  Dcalc.Ast.expr Pos.marked Bindlib.box * ctx

val translate_scope_decl :
  Ast.struct_ctx ->
  Ast.enum_ctx ->
  scope_sigs_ctx ->
  Ast.ScopeName.t ->
  Ast.scope_decl ->
  Dcalc.Ast.expr Pos.marked Bindlib.box * Dcalc.Ast.struct_ctx

val build_scope_typ_from_sig :
  (Ast.ScopeVar.t * Dcalc.Ast.typ) list ->
  Ast.StructName.t ->
  Ast.StructName.t ->
  Pos.t ->
  Dcalc.Ast.typ Pos.marked

val translate_program :
  Ast.program ->
  Ast.ScopeName.t ->
  Dcalc.Ast.program * Dcalc.Ast.expr Pos.marked * Dependency.TVertex.t list
