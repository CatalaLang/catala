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

open Catala_utils

type t = Cmdliner.Cmd.Exit.code Cmdliner.Cmd.t
(** Plugins just provide an additional top-level command *)

(** {2 plugin-facing API} *)

module PluginAPI : sig
  open Cmdliner

  val register_generic : Cmd.info -> Cmd.Exit.code Term.t -> unit
  (** Entry point for the registration of a generic catala subcommand *)

  (** The following are used by [Driver.Plugin] to provide a higher-level
      interface, registering plugins that rely on the [Driver.driver] function. *)

  type 'ast plugin_apply_fun_typ =
    source_file:Pos.input_file ->
    output_file:string option ->
    scope:Shared_ast.ScopeName.t option ->
    'ast ->
    Scopelang.Dependency.TVertex.t list ->
    unit
end

val register : t -> unit

(** {2 catala-facing API} *)

val list : unit -> t list
(** List registered plugins *)

val load_file : string -> unit
(** Load the given plugin (cmo/cma or cmxs file) *)

val load_dir : string -> unit
(** Load all plugins found in the given directory *)

(** {3 Facilities for plugins using the standard driver} *)

type 'ast gen = {
  name : string;
  extension : string;
  apply : 'ast PluginAPI.plugin_apply_fun_typ;
}

type handler =
  | Dcalc of Shared_ast.untyped Dcalc.Ast.program gen
  | Lcalc of Shared_ast.untyped Lcalc.Ast.program gen
  | Scalc of Scalc.Ast.program gen
