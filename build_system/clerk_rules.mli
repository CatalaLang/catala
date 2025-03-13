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

type backend = OCaml | Python | C | Tests

val all_backends : backend list

module Var : sig
  type t = Ninja_utils.Var.t

  val catala_exe : t
  val catala_flags : t
  val make : String.Map.key -> t
  val catala_flags_ocaml : t
  val catala_flags_c : t
  val catala_flags_python : t
  val ocamlc_exe : t
  val ocamlopt_exe : t
  val ocaml_flags : t
  val runtime_ocaml_libs : t
  val cc_exe : t
  val c_flags : t
  val runtime_c_libs : t
  val python : t
  val runtime_python_dir : t
  val all_vars : t String.Map.t
end

val base_bindings :
  catala_exe:File.t option ->
  catala_flags:string list ->
  build_dir:File.t ->
  include_dirs:File.t list ->
  ?vars_override:(string * string) list ->
  ?test_flags:string list ->
  ?enabled_backends:backend list ->
  autotest:bool ->
  unit ->
  Ninja_utils.def list

val ninja_init :
  config_file:File.t option ->
  catala_exe:File.t option ->
  catala_opts:string list ->
  autotest:bool ->
  build_dir:File.t option ->
  include_dirs:File.t list ->
  vars_override:(string * string) list ->
  color:Global.when_enum ->
  debug:bool ->
  ninja_output:File.t option ->
  enabled_backends:backend list ->
  extra:Ninja_utils.def Seq.t ->
  test_flags:string list ->
  (build_dir:File.t ->
  fix_path:(File.t -> File.t) ->
  nin_file:File.t ->
  items:Clerk_scan.item Seq.t ->
  var_bindings:Ninja_utils.Binding.t list ->
  'a) ->
  'a
(** The last argument is a continuation that will be executed upon the generated
    ninja file *)
