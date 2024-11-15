(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria,
   contributors: Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** This module contains definitions of global flags and types used throughout.
    They should be defined from the command-line and never modified afterwards. *)

type file = string
(** File names ; equal to [File.t] but let's avoid cyclic dependencies *)

type raw_file = private file
(** A file name that has not yet been resolved, [options.path_rewrite] must be
    called on it *)

type backend_lang = En | Fr | Pl

(** The usual auto/always/never option argument *)
type when_enum = Auto | Always | Never

(** Format of error and warning messages output by the compiler. *)
type message_format_enum = Human | GNU | Lsp

(** Sources for program input *)
type 'file input_src =
  | FileName of 'file  (** A file path to read from disk *)
  | Contents of string * 'file
      (** A raw string containing the code, and the corresponding (fake)
          filename *)
  | Stdin of 'file
      (** Read from stdin; the specified filename will be used for file lookups,
          error reportings, etc. *)

(** {2 Configuration globals} *)

type options = private {
  mutable input_src : file input_src;
  mutable language : backend_lang option;
  mutable debug : bool;
  mutable color : when_enum;
  mutable message_format : message_format_enum;
  mutable trace : bool;
  mutable plugins_dirs : file list;
  mutable disable_warnings : bool;
  mutable max_prec_digits : int;
  mutable path_rewrite : raw_file -> file;
  mutable stop_on_error : bool;
  mutable no_fail_on_assert : bool;
}
(** Global options, common to all subcommands (note: the fields are internally
    mutable only for purposes of the [globals] toplevel value defined below) *)

val options : options
(** A global definition to the global options is provided for convenience, e.g.
    choosing the proper output in formatting functions. Prefer the use of the
    options returned by the command-line parsing whenever possible. *)

val enforce_options :
  ?input_src:file input_src ->
  ?language:backend_lang option ->
  ?debug:bool ->
  ?color:when_enum ->
  ?message_format:message_format_enum ->
  ?trace:bool ->
  ?plugins_dirs:file list ->
  ?disable_warnings:bool ->
  ?max_prec_digits:int ->
  ?path_rewrite:(raw_file -> file) ->
  ?stop_on_error:bool ->
  ?no_fail_on_assert:bool ->
  unit ->
  options
(** Sets up the global options (side-effect); for specific use-cases only, this
    should never be called from the compiler outside of the [Cli] module. Other
    proper uses include setting up the compiler library when using it directly
    through a specific front-end. *)

val input_src_file : file input_src -> file

val raw_file : string -> raw_file
(** Create a [raw_file], for use directly after parsing from the cmdline *)
