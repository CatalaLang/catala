(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2026 Inria,
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

type module_info = {
  name : string Mark.pos;
  item : Scan.item;
  (* extra_items: Scan.item list; (* e.g. included files *) *)
  targets : String.Set.t;
}

type info = {
  var_bindings : Var.bindings;
  modules_map : module_info String.Map.t;
  targets_map : Clerk_config.target String.Map.t;
  target_deps : Clerk_config.target -> String.Set.t;
      (** returns the names of the given target's dependencies *)
  linking_deps : Scan.item -> string list;
      (** item -> modules, topologically ordered *)
  inclusion_map : Scan.item String.Map.t;
      (** Map from source file names to a module that includes them, for all
          source files that actually get included *)
}
(** Info passed to the callback that shall conclude the Ninja file, once the
    whole file tree has been crawled. The modules and targets map differ from
    the raw configuration information:
    - the Stdlib target is added
    - target contents are all modules to actually include in a given target
    - target dependencies are flattened *)

val empty_info : info
val stdlib_target_name : string

val organise_modules :
  config:Clerk_cli.config ->
  var_bindings:Var.bindings ->
  Scan.item Seq.t ->
  info
(** Takes a (memoised) sequence of items from the stdlib and project tree;
    computes the dependency graphs of targets and modules, and returns a
    [callback_info] *)

val module_backends : info -> string -> Clerk_config.backend list
(** Returns the list of backends supported by a given module by analysing the
    clerk targets it belongs to, or, if none, the clerk targets of its
    dependencies *)
