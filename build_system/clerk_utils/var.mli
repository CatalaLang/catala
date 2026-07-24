(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020-2025 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>,
   Romain Primet <romain.prikmet@inria.fr>

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
open Ninja_utils

include module type of struct
  include Ninja_utils.Var
end

(** {1 Quoting rules}

    Quoting belongs to the boundary a value crosses, never to the stored value;
    the variable's type says what crosses: [Expr.atom t] expands to exactly one
    word (quoted at emission where the consumer requires it), [Expr.t t] to a
    word list (spliced). *)

(** {1 Ninja variable names} *)

(** {2 Global vars: always defined, at toplevel} *)

val ninja_required_version : Expr.atom t
val builddir : Expr.atom t
val clerk_exe : Expr.atom t
val clerk_flags : Expr.t t
val catala_exe : Expr.atom t
val catala_flags : Expr.t t
val runtime : Expr.atom t
val all_vars : String.Set.t

(** {2 Definition spreading different rules} *)

val tdir : Expr.atom t
val includes : Expr.t t

(** {2 Rule vars, Used in specific rules} *)

val input : Expr.atom t
val output : Expr.atom t
val src : Expr.atom t
val dst : Expr.atom t
val class_path : Expr.atom t
val cat_files : Expr.atom t
val test_id : Expr.atom t

(** {1 Utility functions} *)

type bindings = (string * string list) list

val ( ! ) : Expr.atom t -> string
(** Run-time reference to the given variable [!var = "${xvarname}"] *)

module Nj : sig
  include module type of struct
    include Ninja_utils
  end
end

val get_var : bindings -> string -> string list
(** replaces [${xvar}] with its value, recursively *)

val expand_vars : bindings -> string -> string
(** expands [${xvar}] references in the given string *)
