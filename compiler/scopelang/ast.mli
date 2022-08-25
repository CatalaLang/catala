(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Abstract syntax tree of the scope language *)

open Utils
open Shared_ast

(** {1 Identifiers} *)

module ScopeMap : Map.S with type key = ScopeName.t
module SubScopeNameSet : Set.S with type elt = SubScopeName.t
module SubScopeMap : Map.S with type key = SubScopeName.t

module StructFieldMapLift : sig
  val lift_box :
    'a Bindlib.box StructFieldMap.t -> 'a StructFieldMap.t Bindlib.box
end

module EnumConstructorMapLift : sig
  val lift_box :
    'a Bindlib.box EnumConstructorMap.t -> 'a EnumConstructorMap.t Bindlib.box
end

type location = scopelang glocation

module LocationSet : Set.S with type elt = location Marked.pos

(** {1 Abstract syntax tree} *)

type naked_expr = (scopelang, Pos.t) naked_gexpr
type expr = (scopelang, Pos.t) gexpr

module ExprMap : Map.S with type key = expr

val locations_used : expr -> LocationSet.t

(** This type characterizes the three levels of visibility for a given scope
    variable with regards to the scope's input and possible redefinitions inside
    the scope.. *)
type io_input =
  | NoInput
      (** For an internal variable defined only in the scope, and does not
          appear in the input. *)
  | OnlyInput
      (** For variables that should not be redefined in the scope, because they
          appear in the input. *)
  | Reentrant
      (** For variables defined in the scope that can also be redefined by the
          caller as they appear in the input. *)

type io = {
  io_output : bool Marked.pos;
      (** [true] is present in the output of the scope. *)
  io_input : io_input Marked.pos;
}
(** Characterization of the input/output status of a scope variable. *)

type rule =
  | Definition of location Marked.pos * typ * io * expr
  | Assertion of expr
  | Call of ScopeName.t * SubScopeName.t

type scope_decl = {
  scope_decl_name : ScopeName.t;
  scope_sig : (typ * io) ScopeVarMap.t;
  scope_decl_rules : rule list;
}

type program = {
  program_scopes : scope_decl ScopeMap.t;
  program_ctx : decl_ctx;
}
