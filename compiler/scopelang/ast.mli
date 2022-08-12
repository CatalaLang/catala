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
module SubScopeName : Uid.Id with type info = Uid.MarkedString.info
module SubScopeNameSet : Set.S with type elt = SubScopeName.t
module SubScopeMap : Map.S with type key = SubScopeName.t
module ScopeVar : Uid.Id with type info = Uid.MarkedString.info
module ScopeVarSet : Set.S with type elt = ScopeVar.t
module ScopeVarMap : Map.S with type key = ScopeVar.t
module StructFieldMap : Map.S with type key = StructFieldName.t

module StructFieldMapLift : sig
  val lift_box :
    'a Bindlib.box StructFieldMap.t -> 'a StructFieldMap.t Bindlib.box
end

module EnumConstructorMap : Map.S with type key = EnumConstructor.t

module EnumConstructorMapLift : sig
  val lift_box :
    'a Bindlib.box EnumConstructorMap.t -> 'a EnumConstructorMap.t Bindlib.box
end

type location =
  | ScopeVar of ScopeVar.t Marked.pos
  | SubScopeVar of
      ScopeName.t * SubScopeName.t Marked.pos * ScopeVar.t Marked.pos

module LocationSet : Set.S with type elt = location Marked.pos

(** {1 Abstract syntax tree} *)

type typ =
  | TLit of typ_lit
  | TStruct of StructName.t
  | TEnum of EnumName.t
  | TArrow of typ Marked.pos * typ Marked.pos
  | TArray of typ
  | TAny

module Typ : Set.OrderedType with type t = typ

type marked_expr = expr Marked.pos
(** The expressions use the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib}
    library, based on higher-order abstract syntax*)

and expr =
  | ELocation of location
  | EVar of expr Bindlib.var
  | EStruct of StructName.t * marked_expr StructFieldMap.t
  | EStructAccess of marked_expr * StructFieldName.t * StructName.t
  | EEnumInj of marked_expr * EnumConstructor.t * EnumName.t
  | EMatch of marked_expr * EnumName.t * marked_expr EnumConstructorMap.t
  | ELit of Dcalc.Ast.lit
  | EAbs of (expr, marked_expr) Bindlib.mbinder * typ Marked.pos list
  | EApp of marked_expr * marked_expr list
  | EOp of operator
  | EDefault of marked_expr list * marked_expr * marked_expr
  | EIfThenElse of marked_expr * marked_expr * marked_expr
  | EArray of marked_expr list
  | ErrorOnEmpty of marked_expr

module Expr : Set.OrderedType with type t = expr
module ExprMap : Map.S with type key = expr

val locations_used : expr Marked.pos -> LocationSet.t

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
  | Definition of location Marked.pos * typ Marked.pos * io * expr Marked.pos
  | Assertion of expr Marked.pos
  | Call of ScopeName.t * SubScopeName.t

type scope_decl = {
  scope_decl_name : ScopeName.t;
  scope_sig : (typ Marked.pos * io) ScopeVarMap.t;
  scope_decl_rules : rule list;
}

type struct_ctx = (StructFieldName.t * typ Marked.pos) list StructMap.t
type enum_ctx = (EnumConstructor.t * typ Marked.pos) list EnumMap.t

type program = {
  program_scopes : scope_decl ScopeMap.t;
  program_enums : enum_ctx;
  program_structs : struct_ctx;
}

(** {1 Variable helpers} *)

module Var : sig
  type t = expr Bindlib.var

  val make : string -> t
  val compare : t -> t -> int
end

module VarMap : Map.S with type key = Var.t

type vars = expr Bindlib.mvar

val make_var : Var.t Marked.pos -> expr Marked.pos Bindlib.box

val make_abs :
  vars ->
  expr Marked.pos Bindlib.box ->
  typ Marked.pos list ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val make_app :
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box list ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val make_let_in :
  Var.t ->
  typ Marked.pos ->
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box

val make_default :
  ?pos:Pos.t ->
  expr Marked.pos list ->
  expr Marked.pos ->
  expr Marked.pos ->
  expr Marked.pos
(** [make_default ?pos exceptions just cons] builds a term semantically
    equivalent to [<exceptions | just :- cons>] (the [EDefault] constructor)
    while avoiding redundant nested constructions. The position is extracted
    from [just] by default.

    Note that, due to the simplifications taking place, the result might not be
    of the form [EDefault]:

    - [<true :- x>] is rewritten as [x]
    - [<ex | true :- def>], when [def] is a default term [<j :- c>] without
      exceptions, is collapsed into [<ex | def>]
    - [<ex | false :- _>], when [ex] is a single exception, is rewritten as [ex] *)
