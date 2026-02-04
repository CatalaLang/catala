(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria, contributor:
   Romain Primet <romain.primet@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(* Stdlib files embedded for web use. Uses jsoo's /static/ virtual filesystem
   which supports both file I/O and directory operations (Sys.readdir,
   Sys.is_directory). *)

open Js_of_ocaml

let stdlib_path = "/static/stdlib"

let register_stdlib () =
  List.iter
    (fun name ->
      match Stdlib_files.read name with
      | Some content ->
        let path = stdlib_path ^ "/" ^ name in
        Sys_js.create_file ~name:path ~content
      | None ->
        (* This is an internal error - should never happen with correct build *)
        failwith ("Missing embedded stdlib file: " ^ name))
    Stdlib_files.file_list
