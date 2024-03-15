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

open Global

val languages : (string * backend_lang) list

val language_code : backend_lang -> string
(** Returns the lowercase two-letter language code *)

val file_lang : file -> backend_lang
(** Associates a file extension with its corresponding {!type: Global.backend_lang}
    string representation. *)

val input_src_file : file input_src -> file

val reverse_path : ?from_dir:file -> to_dir:file -> file -> file
(** If [to_dir] is a path to a given directory and [f] a path to a file as seen
    from absolute path [from_dir], [reverse_path ~from_dir ~to_dir f] is a path
    leading to [f] from [to_dir]. The results attempts to be relative to
    [to_dir]. *)

(** {2 CLI flags and options} *)

val when_opt: when_enum Cmdliner.Arg.conv

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
  val keep_special_ops : bool Term.t
  val monomorphize_types : bool Term.t
  val dead_value_assignment : bool Term.t
  val no_struct_literals : bool Term.t
  val include_dirs : raw_file list Term.t
  val disable_counterexamples : bool Term.t

  val extra_files : file list Term.t
  (** for the 'latex' command *)

  val lcalc : bool Term.t
  (** for the 'interpret' command *)

  val extension : string option Term.t
  (** for the 'depends' command *)

  val prefix : string option Term.t
  (** for the 'depends' command *)
end

(** {2 Command-line application} *)

val version : string
val info : Cmdliner.Cmd.info

val s_plugins : string
(** Manpage section name for the installed plugins *)

exception Exit_with of int
(** Exit with a specific exit code (but less brutally than [Sys.exit] which
    would bypass all finalisers) *)

(** {2 Other helpers} *)

val exec_dir : file
(** Returns the directory of the currently running executable *)
