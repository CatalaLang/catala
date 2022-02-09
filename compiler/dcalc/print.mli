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

(** Printing functions for the default calculus AST *)

open Utils

(** {1 Helpers} *)

val is_uppercase : CamomileLibraryDefault.Camomile.UChar.t -> bool

val begins_with_uppercase : string -> bool

(** {1 Common syntax highlighting helpers}*)

val format_base_type : Format.formatter -> string -> unit

val format_keyword : Format.formatter -> string -> unit

val format_punctuation : Format.formatter -> string -> unit

val format_operator : Format.formatter -> string -> unit

(** {1 Formatters} *)

val format_uid_list : Format.formatter -> Uid.MarkedString.info list -> unit

val format_tlit : Format.formatter -> Ast.typ_lit -> unit

val format_typ : Ast.decl_ctx -> Format.formatter -> Ast.typ Pos.marked -> unit

val format_lit : Format.formatter -> Ast.lit Pos.marked -> unit

val format_op_kind : Format.formatter -> Ast.op_kind -> unit

val format_binop : Format.formatter -> Ast.binop Pos.marked -> unit

val format_ternop : Format.formatter -> Ast.ternop Pos.marked -> unit

val format_log_entry : Format.formatter -> Ast.log_entry -> unit

val format_unop : Format.formatter -> Ast.unop Pos.marked -> unit

val format_var : Format.formatter -> Ast.Var.t -> unit

val format_expr :
  ?debug:bool (** [true] for debug printing *) ->
  Ast.decl_ctx ->
  Format.formatter ->
  Ast.expr Pos.marked ->
  unit

val format_scope :
  ?debug:bool (** [true] for debug printing *) ->
  Ast.decl_ctx ->
  Format.formatter ->
  Ast.ScopeName.t * Ast.scope_body ->
  unit
