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

type backend = OCaml | Python | C | Java | Tests

val all_backends : backend list
val backend_from_config : Clerk_config.backend -> backend

module Var : sig
  type t = Ninja_utils.Var.t

  val catala_exe : t
  val catala_flags : t
  val make : String.Map.key -> t
  val catala_flags_ocaml : t
  val catala_flags_c : t
  val catala_flags_python : t
  val catala_flags_java : t
  val ocamlc_exe : t
  val ocamlopt_exe : t
  val ocaml_flags : t
  val ocaml_include : t
  val runtime : t
  val cc_exe : t
  val c_flags : t
  val c_include : t
  val python : t
  val javac : t
  val jar : t
  val java : t
  val all_vars : t String.Map.t
end

val base_bindings :
  autotest:bool ->
  enabled_backends:backend list ->
  config:Clerk_cli.config ->
  (Var.t * string list) list

val run_ninja :
  config:Clerk_cli.config ->
  ?enabled_backends:backend list ->
  autotest:bool ->
  ?clean_up_env:bool ->
  ?ninja_flags:string list ->
  (Format.formatter -> Clerk_scan.item list -> (Var.t * string list) list -> 'a) ->
  'a
(** Scan the source tree, run a ninja process, and send to it the expected build
    instructions. A callback can be supplied to retrieve the source items, and
    optionally add entries to the ninja file.

    By default, all backends are enabled, the env is not cleaned of CATALA_*
    variables *)
