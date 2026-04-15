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

open Catala_utils
open Clerk_utils

(** {1 System analysis} *)

(** Some functions that poll the surrounding systems (think [./configure])

    This module is sensitive to the CWD at first use. Therefore it's expected
    that [chdir] has been run beforehand to the project root. *)

let ocaml_runtime_dir : File.t Lazy.t =
  lazy File.(Lazy.force Poll.runtime_dir / "ocaml")

let ocaml_include_and_lib_flags : (string list * string list) Lazy.t =
  lazy
    (let link_libs = ["zarith"] in
     let includes_libs =
       List.map
         (fun lib ->
           match
             File.(check_directory (Lazy.force Poll.ocaml_libdir / lib))
           with
           | None ->
             Message.error
               "Required OCaml library not found at %a.@ Try `opam install %s'"
               File.format
               File.(Lazy.force Poll.ocaml_libdir / lib)
               lib
           | Some d ->
             ( ["-I"; d],
               String.map (function '-' -> '_' | c -> c) lib ^ ".cmxa" ))
         link_libs
     in
     let includes, libs = List.split includes_libs in
     List.concat includes, libs)

let ocaml_include_flags : string list Lazy.t =
  lazy (fst (Lazy.force ocaml_include_and_lib_flags))

let ocaml_link_flags : string list Lazy.t =
  lazy (snd (Lazy.force ocaml_include_and_lib_flags))

let c_runtime_dir : File.t Lazy.t =
  lazy File.(Lazy.force Poll.runtime_dir / "c")

let python_runtime_dir : File.t Lazy.t =
  lazy File.(Lazy.force Poll.runtime_dir / "python" / "src" / "catala")

let java_runtime_dir : File.t Lazy.t =
  lazy File.(Lazy.force Poll.runtime_dir / "java")
