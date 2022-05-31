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

open Utils

(** Abstract syntax tree for the lambda calculus *)

(** {1 Abstract syntax tree} *)

(** The expressions use the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib}
    library, based on higher-order abstract syntax*)

type lit =
  | LBool of bool
  | LInt of Runtime.integer
  | LRat of Runtime.decimal
  | LMoney of Runtime.money
  | LUnit
  | LDate of Runtime.date
  | LDuration of Runtime.duration

type except = ConflictError | EmptyError | NoValueProvided | Crash

type marked_expr = expr Marked.pos

and expr =
  | EVar of expr Bindlib.var
  | ETuple of marked_expr list * Dcalc.Ast.StructName.t option
      (** The [MarkedString.info] is the former struct field name*)
  | ETupleAccess of
      marked_expr
      * int
      * Dcalc.Ast.StructName.t option
      * Dcalc.Ast.typ Marked.pos list
      (** The [MarkedString.info] is the former struct field name *)
  | EInj of
      marked_expr * int * Dcalc.Ast.EnumName.t * Dcalc.Ast.typ Marked.pos list
      (** The [MarkedString.info] is the former enum case name *)
  | EMatch of marked_expr * marked_expr list * Dcalc.Ast.EnumName.t
      (** The [MarkedString.info] is the former enum case name *)
  | EArray of marked_expr list
  | ELit of lit
  | EAbs of (expr, marked_expr) Bindlib.mbinder * Dcalc.Ast.typ Marked.pos list
  | EApp of marked_expr * marked_expr list
  | EAssert of marked_expr
  | EOp of Dcalc.Ast.operator
  | EIfThenElse of marked_expr * marked_expr * marked_expr
  | ERaise of except
  | ECatch of marked_expr * except * marked_expr

type program = { decl_ctx : Dcalc.Ast.decl_ctx; scopes : (expr, Dcalc.Ast.untyped) Dcalc.Ast.scopes }

(** {1 Variable helpers} *)

module Var : sig
  type t = expr Bindlib.var

  val make : string -> t
  val compare : t -> t -> int
end

module VarMap : Map.S with type key = Var.t
module VarSet : Set.S with type elt = Var.t

type vars = expr Bindlib.mvar
type binder = (expr, expr Marked.pos) Bindlib.binder

(** {1 Boxed constructors}*)

val evar : expr Bindlib.var -> Pos.t -> expr Marked.pos Bindlib.box

val etuple :
  expr Marked.pos Bindlib.box list ->
  Dcalc.Ast.StructName.t option ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val etupleaccess :
  expr Marked.pos Bindlib.box ->
  int ->
  Dcalc.Ast.StructName.t option ->
  Dcalc.Ast.typ Marked.pos list ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val einj :
  expr Marked.pos Bindlib.box ->
  int ->
  Dcalc.Ast.EnumName.t ->
  Dcalc.Ast.typ Marked.pos list ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val ematch :
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box list ->
  Dcalc.Ast.EnumName.t ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val earray :
  expr Marked.pos Bindlib.box list -> Pos.t -> expr Marked.pos Bindlib.box

val elit : lit -> Pos.t -> expr Marked.pos Bindlib.box

val eabs :
  (expr, expr Marked.pos) Bindlib.mbinder Bindlib.box ->
  Dcalc.Ast.typ Marked.pos list ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val eapp :
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box list ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val eassert :
  expr Marked.pos Bindlib.box -> Pos.t -> expr Marked.pos Bindlib.box

val eop : Dcalc.Ast.operator -> Pos.t -> expr Marked.pos Bindlib.box

val eifthenelse :
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val ecatch :
  expr Marked.pos Bindlib.box ->
  except ->
  expr Marked.pos Bindlib.box ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val eraise : except -> Pos.t -> expr Marked.pos Bindlib.box

(** {1 Language terms construction}*)

val make_var : Var.t Marked.pos -> expr Marked.pos Bindlib.box

val make_abs :
  vars ->
  expr Marked.pos Bindlib.box ->
  Dcalc.Ast.typ Marked.pos list ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val make_app :
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box list ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val make_let_in :
  Var.t ->
  Dcalc.Ast.typ Marked.pos ->
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val make_multiple_let_in :
  Var.t array ->
  Dcalc.Ast.typ Marked.pos list ->
  expr Marked.pos Bindlib.box list ->
  expr Marked.pos Bindlib.box ->
  Pos.t ->
  expr Marked.pos Bindlib.box

val option_enum : Dcalc.Ast.EnumName.t
val none_constr : Dcalc.Ast.EnumConstructor.t
val some_constr : Dcalc.Ast.EnumConstructor.t

val option_enum_config :
  (Dcalc.Ast.EnumConstructor.t * Dcalc.Ast.typ Marked.pos) list

val make_none : Pos.t -> expr Marked.pos Bindlib.box
val make_some : expr Marked.pos Bindlib.box -> expr Marked.pos Bindlib.box

val make_matchopt_with_abs_arms :
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box

val make_matchopt :
  Pos.t ->
  Var.t ->
  Dcalc.Ast.typ Marked.pos ->
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box ->
  expr Marked.pos Bindlib.box
(** [e' = make_matchopt'' pos v e e_none e_some] Builds the term corresponding
    to [match e with | None -> fun () -> e_none |Some -> fun v -> e_some]. *)

val box_expr : expr Marked.pos -> expr Marked.pos Bindlib.box

(** {1 Special symbols}*)

val handle_default : Var.t
val handle_default_opt : Var.t
