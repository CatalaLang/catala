(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Abstract syntax tree of the desugared representation *)

open Utils

(** {1 Names, Maps and Keys} *)

module IdentMap : Map.S with type key = String.t

module RuleName : Uid.Id with type info = Uid.MarkedString.info

module RuleMap : Map.S with type key = RuleName.t

module RuleSet : Set.S with type elt = RuleName.t

module LabelName : Uid.Id with type info = Uid.MarkedString.info

module LabelMap : Map.S with type key = LabelName.t

module LabelSet : Set.S with type elt = LabelName.t

(** Inside a scope, a definition can refer either to a scope def, or a subscope def *)
module ScopeDef : sig
  type t =
    | Var of Scopelang.Ast.ScopeVar.t
    | SubScopeVar of Scopelang.Ast.SubScopeName.t * Scopelang.Ast.ScopeVar.t

  val compare : t -> t -> int

  val get_position : t -> Pos.t

  val format_t : Format.formatter -> t -> unit

  val hash : t -> int
end

module ScopeDefMap : Map.S with type key = ScopeDef.t

module ScopeDefSet : Set.S with type elt = ScopeDef.t

(** {1 AST} *)

type rule = {
  rule_id : RuleName.t;
  rule_just : Scopelang.Ast.expr Pos.marked Bindlib.box;
  rule_cons : Scopelang.Ast.expr Pos.marked Bindlib.box;
  rule_parameter : (Scopelang.Ast.Var.t * Scopelang.Ast.typ Pos.marked) option;
  rule_exception_to_rules : RuleSet.t Pos.marked;
}

val empty_rule : Pos.t -> Scopelang.Ast.typ Pos.marked option -> rule

val always_false_rule : Pos.t -> Scopelang.Ast.typ Pos.marked option -> rule

type assertion = Scopelang.Ast.expr Pos.marked Bindlib.box

type variation_typ = Increasing | Decreasing

type reference_typ = Decree | Law

type meta_assertion =
  | FixedBy of reference_typ Pos.marked
  | VariesWith of unit * variation_typ Pos.marked option

type scope_def = {
  scope_def_rules : rule RuleMap.t;
  scope_def_typ : Scopelang.Ast.typ Pos.marked;
  scope_def_is_condition : bool;
  scope_def_label_groups : RuleSet.t LabelMap.t;
}

type scope = {
  scope_vars : Scopelang.Ast.ScopeVarSet.t;
  scope_sub_scopes : Scopelang.Ast.ScopeName.t Scopelang.Ast.SubScopeMap.t;
  scope_uid : Scopelang.Ast.ScopeName.t;
  scope_defs : scope_def ScopeDefMap.t;
  scope_assertions : assertion list;
  scope_meta_assertions : meta_assertion list;
}

type program = {
  program_scopes : scope Scopelang.Ast.ScopeMap.t;
  program_enums : Scopelang.Ast.enum_ctx;
  program_structs : Scopelang.Ast.struct_ctx;
}

(** {1 Helpers} *)

val free_variables : rule RuleMap.t -> Pos.t ScopeDefMap.t
