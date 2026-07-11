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
open Clerk_utils

val base_bindings :
  code_coverage:bool ->
  trace:[ `FileName of Catala_utils.Global.raw_file | `Stdout ] option ->
  trace_format:Catala_utils.Global.format_enum option ->
  autotest:bool ->
  enabled_backends:Clerk_config.backend list ->
  inplace:bool ->
  config:Clerk_cli.config ->
  (Var.t * string list) list

exception Stop_ninja

type module_info = {
  name : string Mark.pos;
  item : Scan.item;
  (* extra_items: Scan.item list; (* e.g. included files *) *)
  targets : String.Set.t;
}

type callback_info = {
  var_bindings : (Var.t * string list) list;
  modules_map : module_info String.Map.t;
  targets_map : Clerk_config.target String.Map.t;
  linking_deps : Scan.item -> string list;
      (** item -> modules, topologically ordered *)
}
(** Info passed to the callback that shall conclude the Ninja file, once the
    whole file tree has been crawled. The modules and targets map differ from
    the raw configuration information:
    - the Stdlib target is added
    - target contents are all modules to actually include in a given target
    - target dependencies are flattened *)

val empty_info : callback_info
val stdlib_target_name : string

val run_ninja :
  ?include_dir:bool ->
  config:Clerk_cli.config ->
  ?tests:bool ->
  ?enabled_backends:Clerk_config.backend list ->
  quiet:bool ->
  default:'a ->
  code_coverage:bool ->
  ?trace:[ `FileName of Catala_utils.Global.raw_file | `Stdout ] ->
  ?trace_format:Catala_utils.Global.format_enum ->
  autotest:bool ->
  ?clean_up_env:bool ->
  ?ninja_flags:string list ->
  (Format.formatter -> Scan.item list -> callback_info -> 'a) ->
  'a
(** Scan the source tree, run a ninja process, and send to it the expected build
    instructions. A callback can be supplied to retrieve the source items, and
    optionally add entries to the ninja file.

    As a side-effect, while scanning, on directories that contain sources, any
    catala files that are no longer present in the source tree are removed from
    the corresponding build dir.

    By default, all backends are enabled, the env is not cleaned of CATALA_*
    variables.

    [default] is returned if the callback aborts with exception [Stop_ninja]. *)

val module_supported_backends : callback_info -> string -> Clerk_backends.t
