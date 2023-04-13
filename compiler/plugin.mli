(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, OCamlPro;
   contributors: Louis Gesbert <louis.gesbert@ocamlpro.com>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** {2 catala-facing API} *)

open Catala_utils

type 'ast plugin_apply_fun_typ =
  source_file:Pos.input_file ->
  output_file:string option ->
  scope:string option ->
  'ast ->
  Scopelang.Dependency.TVertex.t list ->
  unit

type 'ast gen = {
  name : string;
  extension : string;
  apply : 'ast plugin_apply_fun_typ;
}

type t =
  | Dcalc of Shared_ast.untyped Dcalc.Ast.program gen
  | Lcalc of Shared_ast.untyped Lcalc.Ast.program gen
  | Scalc of Scalc.Ast.program gen

val find : string -> t
(** Find a registered plugin *)

val load_file : string -> unit
(** Load the given plugin (cmo/cma or cmxs file) *)

val load_dir : string -> unit
(** Load all plugins found in the given directory *)

(** {2 plugin-facing API} *)

module PluginAPI : sig
  val register_dcalc :
    name:string ->
    extension:string ->
    Shared_ast.untyped Dcalc.Ast.program plugin_apply_fun_typ ->
    unit

  val register_lcalc :
    name:string ->
    extension:string ->
    Shared_ast.untyped Lcalc.Ast.program plugin_apply_fun_typ ->
    unit

  val register_scalc :
    name:string ->
    extension:string ->
    Scalc.Ast.program plugin_apply_fun_typ ->
    unit
end

(**/*)

val register : t -> unit
