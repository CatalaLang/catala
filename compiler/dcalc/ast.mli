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

(** Abstract syntax tree of the default calculus intermediate representation *)

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
  | KDuration  (** All ops don't have a KDate and KDuration. *)

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
  | Concat
  | Filter

type log_entry =
  | VarDef of typ
      (** During code generation, we need to know the type of the variable being logged for
          embedding *)
  | BeginCall
  | EndCall
  | PosRecordIfTrueBool

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
  | EAbs of ((expr, expr Pos.marked) Bindlib.mbinder[@opaque]) Pos.marked * typ Pos.marked list
  | EApp of expr Pos.marked * expr Pos.marked list
  | EAssert of expr Pos.marked
  | EOp of operator
  | EDefault of expr Pos.marked list * expr Pos.marked * expr Pos.marked
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked
  | ErrorOnEmpty of expr Pos.marked

type struct_ctx = (StructFieldName.t * typ Pos.marked) list StructMap.t

type enum_ctx = (EnumConstructor.t * typ Pos.marked) list EnumMap.t

type decl_ctx = { ctx_enums : enum_ctx; ctx_structs : struct_ctx }

type binder = (expr, expr Pos.marked) Bindlib.binder

(** This kind annotation signals that the let-binding respects a structural invariant. These
    invariants concern the shape of the expression in the let-binding, and are documented below. *)
type scope_let_kind =
  | DestructuringInputStruct  (** [let x = input.field]*)
  | ScopeVarDefinition  (** [let x = error_on_empty e]*)
  | SubScopeVarDefinition
      (** [let s.x = fun _ -> e] or [let s.x = error_on_empty e] for input-only subscope variables. *)
  | CallingSubScope  (** [let result = s ({ x = s.x; y = s.x; ...}) ]*)
  | DestructuringSubScopeResults  (** [let s.x = result.x ]**)
  | Assertion  (** [let _ = assert e]*)
[@@deriving show]

type scope_let = {
  scope_let_var : expr Bindlib.var Pos.marked;
  scope_let_kind : scope_let_kind;
  scope_let_typ : typ Pos.marked;
  scope_let_expr : expr Pos.marked Bindlib.box;
}
(** A scope let-binding has all the information necessary to make a proper let-binding expression,
    plus an annotation for the kind of the let-binding that comes from the compilation of a
    {!module: Scopelang.Ast} statement. *)

type scope_body = {
  scope_body_lets : scope_let list;
  scope_body_result : expr Pos.marked Bindlib.box;
  scope_body_arg : expr Bindlib.var;
  scope_body_input_struct : StructName.t;
  scope_body_output_struct : StructName.t;
}
(** Instead of being a single expression, we give a little more ad-hoc structure to the scope body
    by decomposing it in an ordered list of let-bindings, and a result expression that uses the
    let-binded variables. *)

type program = { decl_ctx : decl_ctx; scopes : (ScopeName.t * expr Bindlib.var * scope_body) list }

(** {1 Helpers} *)

(** {2 Variables}*)

module Var : sig
  type t = expr Bindlib.var

  val make : string Pos.marked -> t

  val compare : t -> t -> int
end

module VarMap : Map.S with type key = Var.t

val fv : expr Pos.marked -> unit VarMap.t

val free_vars : expr Pos.marked -> Var.t list

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
  Pos.t ->
  expr Pos.marked Bindlib.box

(**{2 Other}*)

val empty_thunked_term : expr Pos.marked

val is_value : expr Pos.marked -> bool

(** {1 AST manipulation helpers}*)

val build_whole_scope_expr : decl_ctx -> scope_body -> Pos.t -> expr Pos.marked Bindlib.box
(** Usage: [build_whole_scope_expr ctx body scope_position] where [scope_position] corresponds to
    the line of the scope declaration for instance. *)

val build_whole_program_expr : program -> ScopeName.t -> expr Pos.marked Bindlib.box
(** Usage: [build_whole_program_expr program main_scope] builds an expression corresponding to the
    main program and returning the main scope as a function. *)

val expr_size : expr Pos.marked -> int
(** Used by the optimizer to know when to stop *)

val variable_types : program -> typ Pos.marked VarMap.t
(** Traverses all the scopes and retrieves all the types for the variables that may appear in scope
    or subscope variable definitions, giving them as a big map. *)
