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

(** This module scans the system for the location of various required tools and
    libraries, to be used as default values. (think [./configure])

    This module is sensitive to the CWD at first use. Therefore it's expected
    that [chdir] has been run beforehand to the project root. *)

val clerk_exe : File.t Lazy.t
val catala_exe : File.t Lazy.t

val ocaml_libdir : File.t Lazy.t
(** Locates the main [lib] directory containing the OCaml libs *)

val ocaml_include_flags : string list Lazy.t
val ocaml_link_flags : string list Lazy.t

val runtime_dir : File.t Lazy.t
(** Locates the directory containing the runtimes to link to (in one
    subdirectory per backend) *)

val stdlib_dir : File.t Lazy.t
(** Locates the directory containing the standard library, including external
    implementations. Equal to [runtime_dir] on a standard installation. *)

val ocaml_runtime_dir : File.t Lazy.t
val c_runtime_dir : File.t Lazy.t
val python_runtime_dir : File.t Lazy.t
val java_runtime_dir : File.t Lazy.t

val catala_source_tree_root : File.t option Lazy.t
(** Set if, and only if the current build happens from within the Catala
    compiler source tree itself. This requires some specific options and
    includes for correct bootstrapping, and running the tests without requiring
    an installation first. *)
