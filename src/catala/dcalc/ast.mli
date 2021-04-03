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

module ScopeName : Uid.Id with type info = Uid.MarkedString.info

module StructName : Uid.Id with type info = Uid.MarkedString.info

module StructFieldName : Uid.Id with type info = Uid.MarkedString.info

module StructMap : Map.S with type key = StructName.t

module EnumName : Uid.Id with type info = Uid.MarkedString.info

module EnumConstructor : Uid.Id with type info = Uid.MarkedString.info

module EnumMap : Map.S with type key = EnumName.t

(** Abstract syntax tree for the default calculus *)

(** {1 Abstract syntax tree} *)

type typ_lit = TBool | TUnit | TInt | TRat | TMoney | TDate | TDuration

type typ =
  | TLit of typ_lit
  | TTuple of typ Pos.marked list * StructName.t option
  | TEnum of typ Pos.marked list * EnumName.t
  | TArrow of typ Pos.marked * typ Pos.marked
  | TArray of typ Pos.marked
  | TAny

type date = Runtime.date

type duration = Runtime.duration

type lit =
  | LBool of bool
  | LEmptyError
  | LInt of Runtime.integer
  | LRat of Runtime.decimal
  | LMoney of Runtime.money
  | LUnit
  | LDate of date
  | LDuration of duration

type op_kind =
  | KInt
  | KRat
  | KMoney
  | KDate
  | KDuration  (** All ops don't have a Kdate and KDuration *)

type ternop = Fold

type binop =
  | And
  | Or
  | Xor
  | Add of op_kind
  | Sub of op_kind
  | Mult of op_kind
  | Div of op_kind
  | Lt of op_kind
  | Lte of op_kind
  | Gt of op_kind
  | Gte of op_kind
  | Eq
  | Neq
  | Map
  | Filter

type log_entry = VarDef | BeginCall | EndCall | PosRecordIfTrueBool

type unop =
  | Not
  | Minus of op_kind
  | Log of log_entry * Utils.Uid.MarkedString.info list
  | Length
  | IntToRat
  | GetDay
  | GetMonth
  | GetYear

type operator = Ternop of ternop | Binop of binop | Unop of unop

(** The expressions use the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib} library, based on
    higher-order abstract syntax*)
type expr =
  | EVar of expr Bindlib.var Pos.marked
  | ETuple of expr Pos.marked list * StructName.t option
      (** The [MarkedString.info] is the former struct field name*)
  | ETupleAccess of expr Pos.marked * int * StructName.t option * typ Pos.marked list
      (** The [MarkedString.info] is the former struct field name *)
  | EInj of expr Pos.marked * int * EnumName.t * typ Pos.marked list
      (** The [MarkedString.info] is the former enum case name *)
  | EMatch of expr Pos.marked * expr Pos.marked list * EnumName.t
      (** The [MarkedString.info] is the former enum case name *)
  | EArray of expr Pos.marked list
  | ELit of lit
  | EAbs of (expr, expr Pos.marked) Bindlib.mbinder Pos.marked * typ Pos.marked list
  | EApp of expr Pos.marked * expr Pos.marked list
  | EAssert of expr Pos.marked
  | EOp of operator
  | EDefault of expr Pos.marked list * expr Pos.marked * expr Pos.marked
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked
  | ErrorOnEmpty of expr Pos.marked

type struct_ctx = (StructFieldName.t * typ Pos.marked) list StructMap.t

type enum_ctx = (EnumConstructor.t * typ Pos.marked) list EnumMap.t

type decl_ctx = { ctx_enums : enum_ctx; ctx_structs : struct_ctx }

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

val make_multiple_let_in :
  Var.t array ->
  typ Pos.marked list ->
  expr Pos.marked list Bindlib.box ->
  expr Pos.marked Bindlib.box ->
  expr Pos.marked Bindlib.box

type binder = (expr, expr Pos.marked) Bindlib.binder

type program = { decl_ctx : decl_ctx; scopes : (ScopeName.t * Var.t * expr Pos.marked) list }
