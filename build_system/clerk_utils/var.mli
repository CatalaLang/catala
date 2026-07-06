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

(** {1 Ninja variable names} *)

(** {2 Global vars: always defined, at toplevel} *)

val ninja_required_version : t
val builddir : t
val clerk_exe : t
val clerk_flags : t
val catala_exe : t
val catala_flags : t
val make : string -> t
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
(** Double-quoted variable reference [quoted var = "\"${xvarname}\""], for use as
    a shell argument in a rule command when the value may contain spaces (e.g. a
    path-valued variable under an install dir with spaces). Not for ninja paths
    in inputs/outputs. *)

val quote_arg : string -> string
(** Like {!quoted} but double-quotes a literal string (e.g. an absolute include
    directory) rather than a variable reference: [quote_arg s = "\"" ^ s ^ "\""].
    Same rationale — protect spaces in a path for the rule command's shell — and
    same restriction: command contexts only, not ninja input/output paths. *)

val unquote : string -> string
(** Inverse of {!quote_arg}: strips one layer of surrounding double quotes if
    present. Use when splicing a shell-quoted variable into a DIRECT argv exec
    (no shell, e.g. the link step), which would otherwise take the quotes
    literally. *)

val get_var : bindings -> t -> string list
(** replaces [${xvar}] with its value, recursively *)

val expand_vars : bindings -> string -> string
(** expands [${xvar}] references in the given string *)
