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

val catala_exe : string option Term.t
val catala_opts : string list Term.t
val autotest : bool Term.t
val build_dir : string option Term.t
val include_dirs : string list Term.t
val test_flags : string list Term.t
val runtest_report : string option Term.t
val runtest_out : string option Term.t
val backend : [> `C | `Interpret | `OCaml | `Python ] Term.t
val ignore_modules : bool Term.t
val run_command : string Term.t
val vars_override : (string * string) list Term.t

module Global : sig
  val color : Catala_utils.Global.when_enum Term.t
  val debug : bool Term.t

  val term :
    (config_file:Catala_utils.File.t option ->
    catala_exe:Catala_utils.File.t option ->
    catala_opts:string list ->
    autotest:bool ->
    build_dir:Catala_utils.File.t option ->
    include_dirs:string list ->
    vars_override:(string * string) list ->
    color:Catala_utils.Global.when_enum ->
    debug:bool ->
    ninja_output:Catala_utils.File.t option ->
    'a) ->
    'a Term.t
end

val files_or_folders : string list Term.t
val files : string list Term.t
val single_file : string Term.t
val reset_test_outputs : bool Term.t
val scope : string option Term.t
val targets : string list Term.t
val report_verbosity : [> `Failures | `Short | `Summary | `Verbose ] Term.t
val report_xml : bool Term.t
val diff_command : string option option Term.t
val ninja_flags : string list Term.t
val info : Cmd.info
