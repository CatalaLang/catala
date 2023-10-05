(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** This file is only for demonstration purposes, showing a trivial use of
    backend plugins for Catala.

    The code for the Python backend already has first-class support, so there
    would be no reason to use this plugin instead *)

open Catala_utils

let run
    includes
    output
    optimize
    check_invariants
    avoid_exceptions
    closure_conversion
    options =
  let open Driver.Commands in
  let prg, _, type_ordering =
    Driver.Passes.scalc options ~includes ~optimize ~check_invariants
      ~avoid_exceptions ~closure_conversion
  in

  let output_file, with_output = get_output_format options ~ext:".py" output in
  Message.emit_debug "Compiling program into Python...";
  Message.emit_debug "Writing to %s..."
    (Option.value ~default:"stdout" output_file);
  with_output @@ fun fmt -> Scalc.To_python.format_program fmt prg type_ordering

let term =
  let open Cmdliner.Term in
  const run
  $ Cli.Flags.include_dirs
  $ Cli.Flags.output
  $ Cli.Flags.optimize
  $ Cli.Flags.check_invariants
  $ Cli.Flags.avoid_exceptions
  $ Cli.Flags.closure_conversion

let () =
  Driver.Plugin.register "python-plugin" term
    ~doc:
      "This plugin is for demonstration purposes and should be equivalent to \
       using the built-in Python backend"
