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

include module type of struct
  include Ninja_utils.Var
end

(** {1 Quoting rules}

    Quoting belongs to the boundary a value crosses, never to the stored value:
    - declaring: holds shell arguments -> {!cmd_only}; names build-statement
      paths -> {!make}. Dual-use (CATALA_EXE): [make], quote each command ref.
    - command ref: expands to one argument -> {!quoted}; to several -> bare
      [!var].
    - command literal: spaceable (a path) -> {!quote_arg}; a flag -> bare.
    - [def] binding values: never contain a quote char ({!binding_words} quotes
      cmd_only vars at emit; direct execs read values as argv).
    - {!get_var} output is argv: use as-is, never quote or strip. *)

(** {1 Ninja variable names} *)

(** {2 Global vars: always defined, at toplevel} *)

val ninja_required_version : t
val builddir : t
val clerk_exe : t
val clerk_flags : t
val catala_exe : t
val catala_flags : t
val make : string -> t

val cmd_only : t -> t
(** [cmd_only (make "V")] declares that ${V} is only ever consumed as shell
    arguments (rule commands, direct-exec argv), never as a build-statement
    path. Its words are shell-quoted when the binding is written to the ninja
    file ({!binding_words}); the stored value stays unquoted for direct argv
    execs. Not for vars with a path consumer (builddir, tdir, CATALA_EXE, ...):
    ninja uses those values as file names, where quotes would be literal.
    A word-list var can never have a path consumer: its binding would need
    per-word quotes for commands and verbatim text for paths at once. Split
    such a var in two (a raw path var, and a derived cmd_only flags var). *)

val is_cmd_only : t -> bool
val runtime : t
val all_vars : t String.Map.t

(** {2 Definition spreading different rules} *)

val tdir : t
val includes : t

(** {2 Rule vars, Used in specific rules} *)

val input : t
val output : t
val src : t
val dst : t
val class_path : t
val cat_files : t
val test_id : t

(** {1 Utility functions} *)

type bindings = (t * string list) list

val ( ! ) : t -> string
(** Run-time reference to the given variable [!var = "${xvarname}"] *)

val quoted : t -> string
(** Double-quote a variable reference for use as a shell argument in a rule
    command (spaced paths). Not for ninja input/output paths. *)

val quote_arg : string -> string
(** Like {!quoted} but for a literal string. Rule-command literals only, never
    binding values (see {!cmd_only}). *)

val binding_words : t -> string list -> string list
(** A binding's words as written to the ninja file: shell-quoted for cmd_only
    vars, verbatim otherwise. *)

val get_var : bindings -> t -> string list
(** replaces [${xvar}] with its value, recursively *)

val expand_vars : bindings -> string -> string
(** expands [${xvar}] references in the given string *)
