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
(** The argument is assumed to be 1-column wide (but can be a multi-char utf8
    character) *)

val op_style : Format.formatter -> string -> unit
val lit_style : Format.formatter -> string -> unit

(** {1 Some basic stringifiers} *)

val operator_to_string : 'a operator -> string
(** Prints the operator symbols with kind suffixes, as expected by the OCaml
    backend (e.g. "+^", "+$", etc.) *)

(** {1 Formatters} *)

val uid_list : Format.formatter -> Uid.MarkedString.info list -> unit
val enum_constructor : Format.formatter -> EnumConstructor.t -> unit
val tlit : Format.formatter -> typ_lit -> unit
val module_name : Format.formatter -> ModuleName.t -> unit
val path : Format.formatter -> ModuleName.t Mark.pos list -> unit
val location : Format.formatter -> 'a glocation -> unit
val external_ref : Format.formatter -> external_ref Mark.pos -> unit
val typ : decl_ctx -> Format.formatter -> typ -> unit
val lit : Format.formatter -> lit -> unit
val operator : ?debug:bool -> Format.formatter -> 'a operator -> unit
val log_entry : Format.formatter -> log_entry -> unit
val except : Format.formatter -> except -> unit
val var : Format.formatter -> 'e Var.t -> unit
val var_debug : Format.formatter -> 'e Var.t -> unit

val expr : ?debug:bool -> unit -> Format.formatter -> ('a, 'm) gexpr -> unit
(** Expression printer.

    @param debug
      (default to the global setting) turns on printing of logging nodes,
      variable indices and operator suffixes. See the interface below for more
      detailed control. *)

(** {2 Generic expression printer interface} *)

module type EXPR_PARAM = sig
  val bypass : Format.formatter -> ('a, 't) gexpr -> bool
  (** can be used to customise printing of any specific nodes or subtrees: will
      cancel normal printing upon returning [true]. *)

  val operator : Format.formatter -> 'a operator -> unit
  val var : Format.formatter -> ('a, 't) gexpr Var.t -> unit
  val lit : Format.formatter -> lit -> unit

  val pre_map : ('a, 't) gexpr -> ('a, 't) gexpr
  (** pre-processing on expressions: can be used to skip log calls, etc. *)
end

module ExprGen (C : EXPR_PARAM) : sig
  val expr : Format.formatter -> ('a, 't) gexpr -> unit
end

module ExprConciseParam : EXPR_PARAM
module ExprDebugParam : EXPR_PARAM

(** {1 Debugging versions that don't require a context} *)

val typ_debug : Format.formatter -> typ -> unit

val scope :
  ?debug:bool ->
  decl_ctx ->
  Format.formatter ->
  ScopeName.t * ('a, 'm) gexpr scope_body ->
  unit

val program : ?debug:bool -> Format.formatter -> ('a, 'm) gexpr program -> unit

(** User-facing, localised printer *)
module UserFacing : sig
  val unit : Cli.backend_lang -> Format.formatter -> Runtime.unit -> unit
  val bool : Cli.backend_lang -> Format.formatter -> Runtime.bool -> unit
  val integer : Cli.backend_lang -> Format.formatter -> Runtime.integer -> unit
  val decimal : Cli.backend_lang -> Format.formatter -> Runtime.decimal -> unit
  val money : Cli.backend_lang -> Format.formatter -> Runtime.money -> unit
  val date : Cli.backend_lang -> Format.formatter -> Runtime.date -> unit

  val duration :
    Cli.backend_lang -> Format.formatter -> Runtime.duration -> unit

  val lit : Cli.backend_lang -> Format.formatter -> lit -> unit
  val lit_to_string : Cli.backend_lang -> lit -> string

  val value :
    ?fallback:(Format.formatter -> ('a, 't) gexpr -> unit) ->
    Cli.backend_lang ->
    Format.formatter ->
    ('a, 't) gexpr ->
    unit
  (** Prints a value in a localised format, intended to be read by an end-user.

      @param fallback
        is called upon non-value expressions (by default, [Invalid_argument] is
        raised) *)

  val expr : Cli.backend_lang -> Format.formatter -> (_, _) gexpr -> unit
  (** This combines the user-facing value printer and the generic expression
      printer to handle all AST nodes *)
end
