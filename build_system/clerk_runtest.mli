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

(** This module contains specific commands used to detect and run inline tests
    in Catala files. The functionality is built into the `clerk runtest`
    subcommand, but is separate from the normal Clerk behaviour: Clerk drives
    Ninja, which in turn might need to evaluate tests as part of some rules and
    can run `clerk runtest` in a reentrant way. *)

val has_inline_tests : string -> bool
(** Checks if the given named file contains inline tests (either directly or
    through includes) *)

val run_inline_tests : reset:bool -> string -> string -> string list -> unit
(** [run_inline_tests ~reset file catala_exe catala_opts] runs the tests in
    Catala [file] using the given path to the Catala executable and the provided
    options. Output is printed to [stdout] if [reset] is false, otherwise [file]
    is replaced with the updated test results. *)
