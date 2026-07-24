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

open Ninja_utils

include module type of struct
  include Ninja_utils.Var
end

(** {1 Quoting rules}

    Quoting belongs to the boundary a value crosses, never to the stored value;
    the variable's type says what crosses: [string t] expands to exactly one
    word (quoted at emission where the consumer requires it), [Expr.t t] to a
    word list (spliced). *)

(** {1 Ninja variable names} *)

(** {2 Global vars: always defined, at toplevel} *)

val ninja_required_version : string t
val builddir : string t
val clerk_exe : string t
val clerk_flags : Expr.t t
val catala_exe : string t
val catala_flags : Expr.t t
val runtime : string t

(** {2 Definition spreading different rules} *)

val tdir : string t
val includes : Expr.t t

(** {2 Rule vars, Used in specific rules} *)

val input : string t
val output : string t
val src : string t
val dst : string t
val class_path : string t
val cat_files : string t
val test_id : string t

(** {1 Utility functions} *)

type bindings = (string * string list) list

val of_words : 'a t -> string list -> 'a
(** Packs kindless outside words (toml/CLI overrides) into a typed payload;
    errors if an atom var receives several words *)

val to_words : 'a t -> 'a -> string list
(** Projection to the string-level env consumed by {!get_var}/direct exec *)

val env_of_bindings : Ninja_utils.Binding.any list -> bindings

val ( ! ) : string t -> string
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
