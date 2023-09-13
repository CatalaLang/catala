(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

type backend_lang = En | Fr | Pl

(** The usual auto/always/never option argument *)
type when_enum = Auto | Always | Never

val when_opt: when_enum Cmdliner.Arg.conv

type message_format_enum =
  | Human
  | GNU  (** Format of error and warning messages output by the compiler. *)

type input_file = FileName of string | Contents of string

val languages : (string * backend_lang) list

val language_code : backend_lang -> string
(** Returns the lowercase two-letter language code *)

(** {2 Configuration globals} *)

type options = private {
  mutable input_file : input_file;
  mutable language : backend_lang option;
  mutable debug : bool;
  mutable color : when_enum;
  mutable message_format : message_format_enum;
  mutable trace : bool;
  mutable plugins_dirs : string list;
  mutable build_dir : string option;
  mutable disable_warnings : bool;
  mutable max_prec_digits : int;
}
(** Global options, common to all subcommands (note: the fields are internally
    mutable only for purposes of the [globals] toplevel value defined below) *)

val globals : options
(** A global definition to the global options is provided for convenience, e.g.
    choosing the proper output in formatting functions. Prefer the use of the
    options returned by the command-line parsing whenever possible. *)

val enforce_globals :
  ?input_file:input_file ->
  ?language:backend_lang option ->
  ?debug:bool ->
  ?color:when_enum ->
  ?message_format:message_format_enum ->
  ?trace:bool ->
  ?plugins_dirs:string list ->
  ?build_dir:string option ->
  ?disable_warnings:bool ->
  ?max_prec_digits:int ->
  unit ->
  options
(** Sets up the global options (side-effect); for specific use-cases only, this
    should never be called from the compiler or when going through normal
    command-line parsing. Proper uses include setting up the compiler library
    when using it directly through a specific front-end. *)

(** {2 CLI flags and options} *)

module Flags : sig
  open Cmdliner

  module Global : sig
    val flags : options Term.t
    (** Global flags available to all commands. Note that parsing this term also
        performs some side-effects into [GlobalRefs] and sets up signal/error
        processing. Sets [input_file] to [FileName "-"], use [options] for the
        full parser *)

    val options : options Term.t
    (** [flags] plus an additional positional argument for the input file *)
  end

  (** Parsers for all flags and options that commands can use *)

  val check_invariants : bool Term.t
  val wrap_weaved_output : bool Term.t
  val print_only_law : bool Term.t
  val ex_scope : string Term.t
  val ex_scope_opt : string option Term.t
  val ex_variable : string Term.t
  val output : string option Term.t
  val optimize : bool Term.t
  val avoid_exceptions : bool Term.t
  val closure_conversion : bool Term.t
  val link_modules : string list Term.t
  val disable_counterexamples : bool Term.t
end

(** {2 Command-line application} *)

val version : string
val info : Cmdliner.Cmd.info

val s_plugins : string
(** Manpage section name for the installed plugins *)

exception Exit_with of int
(** Exit with a specific exit code (but less brutally than [Sys.exit] which
    would bypass all finalisers) *)
