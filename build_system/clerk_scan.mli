(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020 Inria,
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

(** This module is responsible for scanning Catala files, extracting dependency
    and test information. It is based on the lightweight "line-parser"
    ([Surface.Parser_driver.line]) *)

open Catala_utils

type item = {
  file_name : File.t;
  module_def : string Mark.pos option;
  is_stdlib : bool;
  extrnal : bool;
  used_modules : string Mark.pos list;
  included_files : File.t Mark.pos list;
  has_inline_tests : bool;
  has_scope_tests : bool Lazy.t;
}
(** Contains all the data extracted from a single Catala file. Lists are in
    reverse file order. *)

val libcatala : File.t
(** Subdirectory of the build directory holding the runtime and standard library
    and its built artifacts *)

val get_lang : File.t -> Global.backend_lang option
(** Guesses Catala dialect from file-name and global options *)

val catala_file : File.t -> Global.backend_lang -> item
(** Scans a single Catala file into an item *)

val tree : File.t -> (File.t * File.t list * item list) Seq.t
(** Recursively scans a directory, and returns the corresponding subdirectories
    and items in sequence, by directory. *)

val test_command_args : string -> string option
(** Parses a test command-line (in the form "$ catala <args>") and returns the
    arguments as a string, or [None] if there is no match *)

val find_test_scope : lang:Global.backend_lang -> File.t -> bool
(** Checks if the given file contains #[test] scope annotations, recursively
    through file includes. The file extension takes precendence over the [~lang]
    argument. *)

val target_basename : item -> File.t
(** Returns the expected basename (without directory or extension) for artifacts
    based on this file: the module name is used if defined, otherwise the
    original file is used. In both case, it is normalised using [String.to_id]. *)

val target_file_name : item -> File.t
(** Like [target_basename], but returns a relative filename to the build
    directory, without extension *)
