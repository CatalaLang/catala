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

type t = unit Cmdliner.Cmd.t
(** Plugins just provide an additional top-level command *)

(** {2 plugin-facing API} *)

val register :
  Cmdliner.Cmd.info ->
  (Catala_utils.Cli.options -> unit) Cmdliner.Term.t ->
  unit
(** Plugins are registerd as [Cmdliner] commands, which must take at least the
    default global options as arguments (this is required for e.g.
    [--plugins-dirs] to be handled correctly, and for setting debug flags), but
    can add more. *)

(** {2 catala-facing API} *)

val list : unit -> t list
(** List registered plugins *)

val names : unit -> string list
(** List the names of registered plugins *)

val load_file : string -> unit
(** Load the given plugin (cmo/cma or cmxs file) *)

val load_dir : string -> unit
(** Load all plugins found in the given directory *)
