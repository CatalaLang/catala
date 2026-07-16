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

open Clerk_utils
open Catala_utils
open File

(* Backend path/command helpers parameterized by [~win32] so Windows behaviour
   is testable on any host. Internal: the backends specialise it to [Sys.win32]. *)

(* Path-list separator: ';' on Windows (':' collides with the 'C:' drive colon),
   ':' elsewhere. *)
let os_path_sep ~win32 = if win32 then ";" else ":"
let pythonpath ~win32 dirs = String.concat (os_path_sep ~win32) dirs

let classpath ~win32 ~backend include_dirs =
  let open Var in
  String.concat (os_path_sep ~win32)
    ((!tdir / backend)
    :: List.map
         (fun d ->
           (if Filename.is_relative d then !builddir / d else d) / backend)
         include_dirs)

let jar_argfile_content (entries : (string * string) list) : string =
  let quote_path p =
    "\"" ^ String.map (function '\\' -> '/' | c -> c) p ^ "\""
  in
  String.concat "\n"
    (List.concat_map
       (fun (dir, file) -> ["-C"; quote_path dir; quote_path file])
       entries)
