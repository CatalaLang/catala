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

type file = string
(** File names ; equal to [File.t] but let's avoid cyclic dependencies *)

type raw_file
(** A file name that has not yet been resolved, [options.path_rewrite] must be
    called on it *)

type backend_lang = En | Fr | Pl

(** The usual auto/always/never option argument *)
type when_enum = Auto | Always | Never

val when_opt : when_enum Cmdliner.Arg.conv

type message_format_enum =
  | Human
  | GNU  (** Format of error and warning messages output by the compiler. *)

(** Sources for program input *)
type input_src =
  | FileName of file  (** A file path to read from disk *)
  | Contents of string * file
      (** A raw string containing the code, and the corresponding (fake)
          filename *)
  | Stdin of file
      (** Read from stdin; the specified filename will be used for file lookups,
          error reportings, etc. *)

val languages : (string * backend_lang) list

val language_code : backend_lang -> string
(** Returns the lowercase two-letter language code *)

val file_lang : file -> backend_lang
(** Associates a file extension with its corresponding {!type: Cli.backend_lang}
    string representation. *)

val input_src_file : input_src -> file

val reverse_path : ?from_dir:file -> to_dir:file -> file -> file
(** If [to_dir] is a path to a given directory and [f] a path to a file as seen
    from absolute path [from_dir], [reverse_path ~from_dir ~to_dir f] is a path
    leading to [f] from [to_dir]. The results attempts to be relative to
    [to_dir]. *)

(** {2 Configuration globals} *)

type options = private {
  mutable input_src : input_src;
  mutable language : backend_lang option;
  mutable debug : bool;
  mutable color : when_enum;
  mutable message_format : message_format_enum;
  mutable trace : bool;
  mutable plugins_dirs : string list;
  mutable disable_warnings : bool;
  mutable max_prec_digits : int;
  mutable path_rewrite : raw_file -> file;
}
(** Global options, common to all subcommands (note: the fields are internally
    mutable only for purposes of the [globals] toplevel value defined below) *)

val globals : options
(** A global definition to the global options is provided for convenience, e.g.
    choosing the proper output in formatting functions. Prefer the use of the
    options returned by the command-line parsing whenever possible. *)

val enforce_globals :
  ?input_src:input_src ->
  ?language:backend_lang option ->
  ?debug:bool ->
  ?color:when_enum ->
  ?message_format:message_format_enum ->
  ?trace:bool ->
  ?plugins_dirs:string list ->
  ?disable_warnings:bool ->
  ?max_prec_digits:int ->
  ?path_rewrite:(file -> file) ->
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
        processing. Sets [input_src] to [Stdin "-stdin-"], use [options] for the
        full parser *)

    val options : options Term.t
    (** [flags] plus an additional positional argument for the input file *)
  end

  (** Parsers for all flags and options that commands can use *)

  val check_invariants : bool Term.t
  val no_typing : bool Term.t
  val wrap_weaved_output : bool Term.t
  val print_only_law : bool Term.t
  val ex_scope : string Term.t
  val ex_scope_opt : string option Term.t
  val ex_variable : string Term.t
  val output : raw_file option Term.t
  val optimize : bool Term.t
  val avoid_exceptions : bool Term.t
  val closure_conversion : bool Term.t
  val include_dirs : raw_file list Term.t
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
