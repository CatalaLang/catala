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
  (Catala_utils.Global.options -> unit) Cmdliner.Term.t ->
  unit
(** Plugins are registerd as [Cmdliner] commands, which must take at least the
    default global options as arguments (this is required for e.g.
    [--plugins-dirs] to be handled correctly, and for setting debug flags), but
    can add more. *)

val register_subcommands : Cmdliner.Cmd.info -> unit Cmdliner.Cmd.t list -> unit
(** This alternative to [register] allows to register plugins that define
    multiple subcommands (e.g. [catala myplugin subcommand --help]). Be aware
    that all subcommands should take the [Catala_utils.Cli.Flags.Global.options]
    term that handles the [--plugins-dirs] flags and performs some
    initialisations. *)

val register_attribute :
  plugin:string ->
  path:string list ->
  contexts:Desugared.Name_resolution.attribute_context list ->
  (pos:Catala_utils.Pos.t ->
  Shared_ast.attr_value ->
  Catala_utils.Pos.attr option) ->
  unit
(** Used to register support for a new attribute in Catala programs. Supposing
    that plugin [foo] is expected to handle an attribute [foo.bar] attached to
    scope declarations, we could write something like:
    {[
      type Pos.attr += FooBar of ...
      let () =
        Plugin.register_attribute
          ~plugin:"foo" ~path:["bar"]
          ~context:[Desugared.Name_resolution.ScopeDecl]
        @@ fun ~pos v -> match v with
        | ... -> Some (FooBar (...))
        | _ -> Message.delayed_error None ...
    ]}
    then, the attribute will be available as [FooBar] in the following passes ;
    it can be retrieved with [Pos.get_attrs] on the relevant positions. *)

(** {2 catala-facing API} *)

val list : unit -> t list
(** List registered plugins *)

val names : unit -> string list
(** List the names of registered plugins *)

val load_file : string -> unit
(** Load the given plugin (cmo/cma or cmxs file) *)

val load_dir : string -> unit
(** Load all plugins found in the given directory *)

val print_failures : unit -> unit
(** Dynlink errors may be silenced at startup time if not in --debug mode, this
    prints them as warnings *)
