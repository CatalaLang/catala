(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Nicolas Chataing <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
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
module StateName : Uid.Id with type info = Uid.MarkedString.info
module ScopeVar : Uid.Id with type info = Uid.MarkedString.info
module ScopeVarSet : Set.S with type elt = ScopeVar.t
module ScopeVarMap : Map.S with type key = ScopeVar.t

(** Inside a scope, a definition can refer either to a scope def, or a subscope
    def *)
module ScopeDef : sig
  type t =
    | Var of ScopeVar.t * StateName.t option
    | SubScopeVar of Scopelang.Ast.SubScopeName.t * ScopeVar.t

  val compare : t -> t -> int
  val get_position : t -> Pos.t
  val format_t : Format.formatter -> t -> unit
  val hash : t -> int
end

module ScopeDefMap : Map.S with type key = ScopeDef.t
module ScopeDefSet : Set.S with type elt = ScopeDef.t

(** {1 AST} *)

(**{2 Expressions}*)
type location =
  | ScopeVar of ScopeVar.t Pos.marked * StateName.t option
  | SubScopeVar of
      Scopelang.Ast.ScopeName.t
      * Scopelang.Ast.SubScopeName.t Pos.marked
      * ScopeVar.t Pos.marked

module LocationSet : Set.S with type elt = location Pos.marked

(** The expressions use the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib}
    library, based on higher-order abstract syntax*)
type expr =
  | ELocation of location
  | EVar of expr Bindlib.var Pos.marked
  | EStruct of
      Scopelang.Ast.StructName.t
      * expr Pos.marked Scopelang.Ast.StructFieldMap.t
  | EStructAccess of
      expr Pos.marked
      * Scopelang.Ast.StructFieldName.t
      * Scopelang.Ast.StructName.t
  | EEnumInj of
      expr Pos.marked
      * Scopelang.Ast.EnumConstructor.t
      * Scopelang.Ast.EnumName.t
  | EMatch of
      expr Pos.marked
      * Scopelang.Ast.EnumName.t
      * expr Pos.marked Scopelang.Ast.EnumConstructorMap.t
  | ELit of Dcalc.Ast.lit
  | EAbs of
      (expr, expr Pos.marked) Bindlib.mbinder Pos.marked
      * Scopelang.Ast.typ Pos.marked list
  | EApp of expr Pos.marked * expr Pos.marked list
  | EOp of Dcalc.Ast.operator
  | EDefault of expr Pos.marked list * expr Pos.marked * expr Pos.marked
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked
  | EArray of expr Pos.marked list
  | ErrorOnEmpty of expr Pos.marked

(** {2 Variable helpers} *)

module Var : sig
  type t = expr Bindlib.var

  val make : string Pos.marked -> t
  val compare : t -> t -> int
end

module VarMap : Map.S with type key = Var.t

type vars = expr Bindlib.mvar

val make_var : Var.t Pos.marked -> expr Pos.marked Bindlib.box

val make_abs :
  vars ->
  expr Pos.marked Bindlib.box ->
  Pos.t ->
  Scopelang.Ast.typ Pos.marked list ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val make_app :
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box list ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val make_let_in :
  Var.t ->
  Scopelang.Ast.typ Pos.marked ->
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box

(** {2 Rules and scopes}*)

type rule = {
  rule_id : RuleName.t;
  rule_just : expr Pos.marked Bindlib.box;
  rule_cons : expr Pos.marked Bindlib.box;
  rule_parameter : (Var.t * Scopelang.Ast.typ Pos.marked) option;
  rule_exception_to_rules : RuleSet.t Pos.marked;
}

val empty_rule : Pos.t -> Scopelang.Ast.typ Pos.marked option -> rule
val always_false_rule : Pos.t -> Scopelang.Ast.typ Pos.marked option -> rule

type assertion = expr Pos.marked Bindlib.box
type variation_typ = Increasing | Decreasing
type reference_typ = Decree | Law

type meta_assertion =
  | FixedBy of reference_typ Pos.marked
  | VariesWith of unit * variation_typ Pos.marked option

type scope_def = {
  scope_def_rules : rule RuleMap.t;
  scope_def_typ : Scopelang.Ast.typ Pos.marked;
  scope_def_is_condition : bool;
  scope_def_io : Scopelang.Ast.io;
  scope_def_label_groups : RuleSet.t LabelMap.t;
}

type var_or_states = WholeVar | States of StateName.t list

type scope = {
  scope_vars : var_or_states ScopeVarMap.t;
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

val locations_used : expr Pos.marked -> LocationSet.t
val free_variables : rule RuleMap.t -> Pos.t ScopeDefMap.t
