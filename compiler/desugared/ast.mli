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
open Shared_ast

(** {1 Names, Maps and Keys} *)

module IdentMap : Map.S with type key = String.t
module RuleName : Uid.Id with type info = Uid.MarkedString.info
module RuleMap : Map.S with type key = RuleName.t
module RuleSet : Set.S with type elt = RuleName.t
module LabelName : Uid.Id with type info = Uid.MarkedString.info
module LabelMap : Map.S with type key = LabelName.t
module LabelSet : Set.S with type elt = LabelName.t

(** Inside a scope, a definition can refer either to a scope def, or a subscope
    def *)
module ScopeDef : sig
  type t =
    | Var of ScopeVar.t * StateName.t option
    | SubScopeVar of SubScopeName.t * ScopeVar.t * Pos.t

  val compare : t -> t -> int
  val get_position : t -> Pos.t
  val format_t : Format.formatter -> t -> unit
  val hash : t -> int
end

module ScopeDefMap : Map.S with type key = ScopeDef.t
module ScopeDefSet : Set.S with type elt = ScopeDef.t

(** {1 AST} *)

(** {2 Expressions} *)

type expr = (desugared, untyped mark) gexpr
(** See {!type:Shared_ast.naked_gexpr} for the complete definition *)

type location = desugared glocation

module LocationSet : Set.S with type elt = location Marked.pos
module ExprMap : Map.S with type key = expr

(** {2 Rules and scopes}*)

type exception_situation =
  | BaseCase
  | ExceptionToLabel of LabelName.t Marked.pos
  | ExceptionToRule of RuleName.t Marked.pos

type label_situation = ExplicitlyLabeled of LabelName.t Marked.pos | Unlabeled

type rule = {
  rule_id : RuleName.t;
  rule_just : expr Bindlib.box;
  rule_cons : expr Bindlib.box;
  rule_parameter : (expr Var.t * typ) option;
  rule_exception : exception_situation;
  rule_label : label_situation;
}

module Rule : Set.OrderedType with type t = rule

val empty_rule : Pos.t -> typ option -> rule
val always_false_rule : Pos.t -> typ option -> rule

type assertion = expr Bindlib.box
type variation_typ = Increasing | Decreasing
type reference_typ = Decree | Law

type meta_assertion =
  | FixedBy of reference_typ Marked.pos
  | VariesWith of unit * variation_typ Marked.pos option

type scope_def = {
  scope_def_rules : rule RuleMap.t;
  scope_def_typ : typ;
  scope_def_is_condition : bool;
  scope_def_io : Scopelang.Ast.io;
}

type var_or_states = WholeVar | States of StateName.t list

type scope = {
  scope_vars : var_or_states ScopeVarMap.t;
  scope_sub_scopes : ScopeName.t Scopelang.Ast.SubScopeMap.t;
  scope_uid : ScopeName.t;
  scope_defs : scope_def ScopeDefMap.t;
  scope_assertions : assertion list;
  scope_meta_assertions : meta_assertion list;
}

type program = {
  program_scopes : scope Scopelang.Ast.ScopeMap.t;
  program_ctx : decl_ctx;
}

(** {1 Helpers} *)

val locations_used : expr -> LocationSet.t
val free_variables : rule RuleMap.t -> Pos.t ScopeDefMap.t
