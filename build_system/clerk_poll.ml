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

(** {1 System analysis} *)

(** Some functions that poll the surrounding systems (think [./configure])

    This module is sensitive to the CWD at first use. Therefore it's expected
    that [chdir] has been run beforehand to the project root. *)

let root = lazy (Sys.getcwd ())

(** Scans for a parent directory being the root of the Catala source repo *)
let catala_project_root : File.t option Lazy.t =
  root
  |> Lazy.map
     @@ fun root ->
     if File.(exists (root / "catala.opam") && exists (root / "dune-project"))
     then Some root
     else None

let exec_dir : File.t = Catala_utils.Cli.exec_dir
let clerk_exe : File.t Lazy.t = lazy (Unix.realpath Sys.executable_name)

let catala_exe : File.t Lazy.t =
  lazy
    (let f = File.(exec_dir / "catala") in
     if Sys.file_exists f then Unix.realpath f
     else
       match catala_project_root with
       | (lazy (Some root)) ->
         Unix.realpath
           File.(root / "_build" / "default" / "compiler" / "catala.exe")
       | _ -> File.check_exec "catala")

(** Locates the main [lib] directory containing the OCaml libs *)
let ocaml_libdir : File.t Lazy.t =
  lazy
    (try String.trim (File.process_out "opam" ["var"; "lib"])
     with Failure _ -> (
       try String.trim (File.process_out "ocamlc" ["-where"])
       with Failure _ -> (
         match File.(check_directory (exec_dir /../ "lib")) with
         | Some d -> d
         | None ->
           Message.error
             "Could not locate the OCaml library directory, make sure OCaml or \
              opam is installed")))

(** Locates the directory containing the OCaml runtime to link to *)
let ocaml_runtime_dir : File.t Lazy.t =
  lazy
    (let d =
       match Lazy.force catala_project_root with
       | Some root ->
         (* Relative dir when running from catala source *)
         File.(
           root
           / "_build"
           / "install"
           / "default"
           / "lib"
           / "catala"
           / "runtime_ocaml")
       | None -> (
         match
           File.check_directory
             File.(exec_dir /../ "lib" / "catala" / "runtime_ocaml")
         with
         | Some d -> d
         | None -> File.(Lazy.force ocaml_libdir / "catala" / "runtime_ocaml"))
     in
     match File.check_directory d with
     | Some dir ->
       Message.debug "Catala runtime libraries found at @{<bold>%s@}." dir;
       dir
     | None ->
       Message.error
         "@[<hov>Could not locate the Catala runtime library at %s.@ Make sure \
          that either catala is correctly installed,@ or you are running from \
          the root of a compiled source tree.@]"
         d)

let ocaml_include_and_lib_flags : (string list * string list) Lazy.t =
  lazy
    (let link_libs = ["zarith"; "dates_calc"] in
     let includes_libs =
       List.map
         (fun lib ->
           match File.(check_directory (Lazy.force ocaml_libdir / lib)) with
           | None ->
             Message.error
               "Required OCaml library not found at %a.@ Try `opam install %s'"
               File.format
               File.(Lazy.force ocaml_libdir / lib)
               lib
           | Some d ->
             ( ["-I"; d],
               String.map (function '-' -> '_' | c -> c) lib ^ ".cmxa" ))
         link_libs
     in
     let includes, libs = List.split includes_libs in
     ( List.concat includes @ ["-I"; Lazy.force ocaml_runtime_dir],
       libs @ [File.(Lazy.force ocaml_runtime_dir / "runtime_ocaml.cmxa")] ))

let ocaml_include_flags : string list Lazy.t =
  lazy (fst (Lazy.force ocaml_include_and_lib_flags))

let ocaml_link_flags : string list Lazy.t =
  lazy (snd (Lazy.force ocaml_include_and_lib_flags))

let c_runtime_dir : File.t Lazy.t =
  lazy File.(Lazy.force ocaml_runtime_dir /../ "runtime_c")

let python_runtime_dir : File.t Lazy.t =
  lazy File.(Lazy.force ocaml_runtime_dir /../ "runtime_python" / "src")
