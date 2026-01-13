(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020-2025 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Cmdliner
open Catala_utils

val catala_exe : string option Term.t
val catala_opts : string list Term.t
val autotest : bool Term.t
val prepare_only : bool Term.t
val quiet : bool Term.t
val build_dir : string option Term.t
val include_dirs : string list Term.t
val test_flags : string list Term.t
val runtest_report : string option Term.t
val runtest_out : string option Term.t
val backend : [> `C | `Interpret | `OCaml | `Python | `Java ] Term.t
val run_command : string Term.t
val vars_override : (string * string) list Term.t
val files_or_folders : string list Term.t
val files : string list Term.t
val targets : string list Term.t
val single_file : string Term.t
val reset_test_outputs : bool Term.t
val scope : string Term.t
val scope_opt : string option Term.t
val scope_input : Yojson.Safe.t option Term.t
val clerk_targets_or_files : string list Term.t
val clerk_targets_or_files_or_folders : string list Term.t
val report_verbosity : [> `Failures | `Short | `Summary | `Verbose ] Term.t
val report_format : [> `Terminal | `JUnitXML | `VSCodeJSON ] Term.t
val code_coverage : bool Term.t
val diff_command : string option option Term.t
val ninja_flags : string list Term.t
val whole_program : bool Term.t
val info : Cmd.info

val color : Global.when_enum Term.t
(** Already included in [init_term] *)

val debug : bool Term.t
(** Already included in [init_term] *)

(** {2 Initialisation of options} *)

type config = {
  options : Clerk_config.t;
  fix_path : File.t -> File.t;
  ninja_file : File.t option;
  test_flags : string list;
}

val init_term : ?allow_test_flags:bool -> unit -> config Term.t
(** Reads the supplied command-line flags and configuration file and runs
    globals initialisation routines *)
