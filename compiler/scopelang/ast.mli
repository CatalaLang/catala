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

(** Abstract syntax tree of the scope language *)

open Utils

(** {1 Identifiers} *)

module ScopeName = Dcalc.Ast.ScopeName

module ScopeNameSet : Set.S with type elt = ScopeName.t

module ScopeMap : Map.S with type key = ScopeName.t

module SubScopeName : Uid.Id with type info = Uid.MarkedString.info

module SubScopeNameSet : Set.S with type elt = SubScopeName.t

module SubScopeMap : Map.S with type key = SubScopeName.t

module ScopeVar : Uid.Id with type info = Uid.MarkedString.info

module ScopeVarSet : Set.S with type elt = ScopeVar.t

module ScopeVarMap : Map.S with type key = ScopeVar.t

module StructName = Dcalc.Ast.StructName
module StructMap = Dcalc.Ast.StructMap
module StructFieldName = Dcalc.Ast.StructFieldName

module StructFieldMap : Map.S with type key = StructFieldName.t

module StructFieldMapLift : sig
  val lift_box : 'a Bindlib.box StructFieldMap.t -> 'a StructFieldMap.t Bindlib.box
end

module EnumName = Dcalc.Ast.EnumName
module EnumMap = Dcalc.Ast.EnumMap
module EnumConstructor = Dcalc.Ast.EnumConstructor

module EnumConstructorMap : Map.S with type key = EnumConstructor.t

module EnumConstructorMapLift : sig
  val lift_box : 'a Bindlib.box EnumConstructorMap.t -> 'a EnumConstructorMap.t Bindlib.box
end

type location =
  | ScopeVar of ScopeVar.t Pos.marked
  | SubScopeVar of ScopeName.t * SubScopeName.t Pos.marked * ScopeVar.t Pos.marked

module LocationSet : Set.S with type elt = location Pos.marked

(** {1 Abstract syntax tree} *)

type typ =
  | TLit of Dcalc.Ast.typ_lit
  | TStruct of StructName.t
  | TEnum of EnumName.t
  | TArrow of typ Pos.marked * typ Pos.marked
  | TArray of typ
  | TAny

(** The expressions use the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib} library, based on
    higher-order abstract syntax*)
type expr =
  | ELocation of location
  | EVar of expr Bindlib.var Pos.marked
  | EStruct of StructName.t * expr Pos.marked StructFieldMap.t
  | EStructAccess of expr Pos.marked * StructFieldName.t * StructName.t
  | EEnumInj of expr Pos.marked * EnumConstructor.t * EnumName.t
  | EMatch of expr Pos.marked * EnumName.t * expr Pos.marked EnumConstructorMap.t
  | ELit of Dcalc.Ast.lit
  | EAbs of (expr, expr Pos.marked) Bindlib.mbinder Pos.marked * typ Pos.marked list
  | EApp of expr Pos.marked * expr Pos.marked list
  | EOp of Dcalc.Ast.operator
  | EDefault of expr Pos.marked list * expr Pos.marked * expr Pos.marked
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked
  | EArray of expr Pos.marked list
  | ErrorOnEmpty of expr Pos.marked

val locations_used : expr Pos.marked -> LocationSet.t

(** This type characterizes the three levels of visibility for a given scope variable with regards
    to the scope's input and possible redefinitions inside the scope.. *)
type io_input =
  | NoInput
      (** For an internal variable defined only in the scope, and does not appear in the input. *)
  | OnlyInput
      (** For variables that should not be redefined in the scope, because they appear in the input. *)
  | Reentrant
      (** For variables defined in the scope that can also be redefined by the caller as they appear
          in the input. *)

type io = {
  io_output : bool Pos.marked;  (** [true] is present in the output of the scope. *)
  io_input : io_input Pos.marked;
}
(** Characterization of the input/output status of a scope variable. *)

type rule =
  | Definition of location Pos.marked * typ Pos.marked * io * expr Pos.marked
  | Assertion of expr Pos.marked
  | Call of ScopeName.t * SubScopeName.t

type scope_decl = {
  scope_decl_name : ScopeName.t;
  scope_sig : (typ Pos.marked * io) ScopeVarMap.t;
  scope_decl_rules : rule list;
}

type struct_ctx = (StructFieldName.t * typ Pos.marked) list StructMap.t

type enum_ctx = (EnumConstructor.t * typ Pos.marked) list EnumMap.t

type program = {
  program_scopes : scope_decl ScopeMap.t;
  program_enums : enum_ctx;
  program_structs : struct_ctx;
}

(** {1 Variable helpers} *)

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
  typ Pos.marked list ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val make_app :
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box list ->
  Pos.t ->
  expr Pos.marked Bindlib.box

val make_let_in :
  Var.t ->
  typ Pos.marked ->
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box
