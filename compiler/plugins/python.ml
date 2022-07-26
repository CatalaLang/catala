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

let name = "python-plugin"
let extension = ".py"

let apply ~output_file ~scope prgm type_ordering =
  ignore scope;
  Utils.File.with_formatter_of_opt_file output_file @@ fun fmt ->
  Scalc.To_python.format_program fmt prgm type_ordering

let () = Driver.Plugin.register_scalc ~name ~extension apply
