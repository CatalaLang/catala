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

type 'ast gen = {
  name : string;
  extension : string;
  apply : string option -> 'ast -> Scopelang.Dependency.TVertex.t list -> unit;
}

type t =
  | Lcalc of Dcalc.Ast.typed Lcalc.Ast.program gen
  | Scalc of Scalc.Ast.program gen

val find : string -> t
(** Find a registered plugin *)

val load_file : string -> unit
(** Load the given plugin (cmo/cma or cmxs file) *)

val load_dir : string -> unit
(** Load all plugins found in the given directory *)

(** {2 plugin-facing API} *)

module PluginAPI : sig
  val register_lcalc :
    name:string ->
    extension:string ->
    (string option ->
    Dcalc.Ast.typed Lcalc.Ast.program ->
    Scopelang.Dependency.TVertex.t list ->
    unit) ->
    unit

  val register_scalc :
    name:string ->
    extension:string ->
    (string option ->
    Scalc.Ast.program ->
    Scopelang.Dependency.TVertex.t list ->
    unit) ->
    unit
end

(**/*)

val register : t -> unit
