(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2024 Inria,
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

open Catala_utils

type backend = ..
type backend += C | OCaml | Java | Python

val register_backend : name:string -> backend -> unit

type doc_backend = Html | Latex

type global = {
  include_dirs : File.t list;
  build_dir : File.t;
  target_dir : File.t;
  catala_exe : File.t option;
  catala_opts : string list;
  default_targets : string list;
}

type target = {
  tname : string;
  tmodules : string list;
  ttests : File.t list;
  backends : backend list;
  include_runtime : bool;
}

type doc = {
  name : string;
  kind : doc_backend;
  entrypoints : string list;
  doc_options : string list;
}

type custom_rule = {
  backend : backend;
  in_exts : string list; (* ocaml/%.cmi *)
  out_exts : string list; (* ocaml/%.cma *)
  commandline : string list;
      (* ${OCAMLOPT_EXE} ${OCAML_FLAGS} -I ${dir} ${in} -a -o ${out} *)
}

type config_file = {
  global : global;
  variables : (string * string list) list;
  targets : target list;
  docs : doc list;
  custom_rules : custom_rule list;
}

type t = config_file

val default_config : t
val read : File.t -> t
val write : File.t -> t -> unit
