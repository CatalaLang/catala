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

(** Printing functions for the default calculus AST *)

open Catala_utils
open Definitions

(** {1 Common syntax highlighting helpers}*)

val base_type : Format.formatter -> string -> unit
val keyword : Format.formatter -> string -> unit
val punctuation : Format.formatter -> string -> unit
val op_style : Format.formatter -> string -> unit
val lit_style : Format.formatter -> string -> unit

(** {1 Formatters} *)

val uid_list : Format.formatter -> Uid.MarkedString.info list -> unit
val enum_constructor : Format.formatter -> EnumConstructor.t -> unit
val tlit : Format.formatter -> typ_lit -> unit
val location : Format.formatter -> 'a glocation -> unit
val typ : decl_ctx -> Format.formatter -> typ -> unit
val lit : Format.formatter -> lit -> unit
val operator : Format.formatter -> 'a operator -> unit
val log_entry : Format.formatter -> log_entry -> unit
val except : Format.formatter -> except -> unit
val var : Format.formatter -> 'e Var.t -> unit
val var_debug : Format.formatter -> 'e Var.t -> unit

val expr :
  ?debug:bool (** [true] for debug printing *) ->
  decl_ctx ->
  Format.formatter ->
  ('a, 'm mark) gexpr ->
  unit

(** {1 Debugging versions that don't require a context} *)

val expr_debug :
  ?debug:bool (** [true] for debug printing *) ->
  Format.formatter ->
  ('a, 'm mark) gexpr ->
  unit

val typ_debug : Format.formatter -> typ -> unit
