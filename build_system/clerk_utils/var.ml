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

include Ninja_utils.Var
(** Ninja variable names *)

(** Global vars: always defined, at toplevel *)

let ninja_required_version = make "ninja_required_version"
let builddir = make "builddir"
let clerk_exe = make "CLERK_EXE"
let clerk_flags = make "CLERK_FLAGS"
let catala_exe = make "CATALA_EXE"
let catala_flags = make "CATALA_FLAGS"

let make, all_vars_ref =
  let all_vars_ref = ref String.Map.empty in
  ( (fun s ->
      let v = make s in
      all_vars_ref := String.Map.add s v !all_vars_ref;
      v),
    all_vars_ref )

let catala_flags_ocaml = make "CATALA_FLAGS_OCAML"
let catala_flags_c = make "CATALA_FLAGS_C"
let catala_flags_python = make "CATALA_FLAGS_PYTHON"
let catala_flags_java = make "CATALA_FLAGS_JAVA"
let ocamlc_exe = make "OCAMLC_EXE"
let ocamlopt_exe = make "OCAMLOPT_EXE"
let ocaml_flags = make "OCAML_FLAGS"
let ocaml_include = make "OCAML_INCLUDE"
let runtime = make "CATALA_RUNTIME"
let cc_exe = make "CC"
let c_flags = make "CFLAGS"
let c_include = make "C_INCLUDE_FLAGS"
let python = make "PYTHON"
let javac = make "JAVAC"
let javac_flags = make "JAVAC_FLAGS"
let jar = make "jar"
let java = make "JAVA"
let all_vars = all_vars_ref.contents

(* Definition spreading different rules *)

let tdir = make "tdir"
let includes = make "includes"

(* Rule vars, Used in specific rules *)

let input = make "in"
let output = make "out"
let src = make "src"
let dst = make "dst"
let class_path = make "class_path"
let cat_files = make "cat_files" (* Useful on Windows only *)

(* let scope = make "scope" *)
let test_id = make "test-id"
let ( ! ) = Ninja_utils.Var.v
