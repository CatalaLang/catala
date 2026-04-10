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

module type S = sig
  val name : string
  val module_ext : string
  val subdir : string
  val src_extensions : string list
  val obj_extensions : string list
  val runtime_targets : only_source:bool -> string list

  module Flags : sig
    val default :
      variables:(string * string list) list ->
      autotest:bool ->
      use_default_flags:bool ->
      test_flags:string list ->
      include_dirs:string list ->
      (Var.t * string list) list
  end

  val modfile :
    is_stdlib:bool ->
    ?suffix:string ->
    (string * string) list ->
    string ->
    string ->
    string

  val static_base_rules : string list -> Ninja_utils.def list

  val runtime_build_statements :
    options:Clerk_lib.Clerk_config.t -> stdbase:string -> Ninja_utils.def list

  val external_copy : Scan.item -> Ninja_utils.def Seq.t

  val catala :
    ?vars:(Var.t * Ninja_utils.Expr.t) list ->
    is_stdlib:bool ->
    inputs:Ninja_utils.Expr.t ->
    implicit_in:Ninja_utils.Expr.t ->
    bool ->
    Ninja_utils.def Seq.t

  val build_object :
    externls:string list ->
    include_dirs:string list ->
    same_dir_modules:(string * string) list ->
    item:Scan.item ->
    bool ->
    Ninja_utils.def Seq.t

  val expose_module :
    same_dir_modules:(string * string) list ->
    used_modules:string list ->
    Ninja_utils.def list

  val copy_to_target :
    build_dir:string ->
    prefix_dir:string ->
    target:Clerk_lib.Clerk_config.target ->
    install_targets:(Clerk_lib.Clerk_config.backend * string) list ->
    unit

  val runtime_dir : File.t Lazy.t

  val extra_rules :
    externls:string list ->
    stdlib_tree:(string * string list * Scan.item list) Seq.t ->
    project_tree:(string * string list * Scan.item list) Seq.t ->
    string list ->
    Ninja_utils.def list

  val extra_default : string list
end
