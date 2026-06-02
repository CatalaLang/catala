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

val get_var : bindings -> t -> string list
(** replaces [${xvar}] with its value, recursively *)

val expand_vars : bindings -> string -> string
(** expands [${xvar}] references in the given string *)
