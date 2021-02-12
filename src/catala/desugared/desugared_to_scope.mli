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

(** Translation from {!module: Desugared.Ast} to {!module: Scopelang.Ast} *)

open Utils

(** {1 Rule tree construction} *)

type rule_tree = Leaf of Ast.rule | Node of rule_tree list * Ast.rule

(** Transforms a flat list of rules into a tree, taking into account the priorities declared between
    rules *)
val def_map_to_tree : Ast.ScopeDef.t -> Ast.rule Ast.RuleMap.t -> rule_tree list

(** From the {!type: rule_tree}, builds an {!constructor: Dcalc.Ast.EDefault} expression in the
    scope language. The [~toplevel] parameter is used to know when to place the toplevel binding in
    the case of functions. *)
val rule_tree_to_expr :
  toplevel:bool -> Pos.t -> Scopelang.Ast.Var.t option -> rule_tree ->
  Scopelang.Ast.expr Pos.marked Bindlib.box


(** {1 AST translation} *)

(** Translates a definition inside a scope, the resulting expression should be an {!constructor:
    Dcalc.Ast.EDefault} *)
val translate_def :
  Ast.ScopeDef.t ->
  Ast.rule Ast.RuleMap.t ->
  Scopelang.Ast.typ Pos.marked ->
  bool ->
  Scopelang.Ast.expr Pos.marked

val translate_scope : Ast.scope -> Scopelang.Ast.scope_decl

(** {1 API} *)

val translate_program : Ast.program -> Scopelang.Ast.program
