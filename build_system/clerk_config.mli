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
type backend += C | OCaml

val register_backend : name:string -> backend -> unit

type doc_backend = Html | Latex

type global = {
  include_dirs : string list;
  build_dir : string;
  catala_opts : string list;
}

type module_ = {
  name : string;
  module_uses : [ `Simple of string | `With_alias of string * string ] list;
  includes : string list;
}

type target = {
  name : string;
  entrypoints : string list;
  backend : backend;
  backend_options : string list;
}

type doc = {
  name : string;
  kind : doc_backend;
  entrypoints : string list;
  doc_options : string list;
}

type config_file = {
  global : global;
  modules : module_ list;
  targets : target list;
  docs : doc list;
}

type t = config_file

val default_config : t
val read : File.t -> t
val write : File.t -> t -> unit
