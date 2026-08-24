(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020-2025 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>,
   Romain Primet <romain.primet@inria.fr>

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

type bindings = Ninja_utils.Binding.any list

val binding_of_words : 'a t -> string list -> Ninja_utils.Binding.any
(** Packs flat words into a typed payload; errors if a scalar var receives
    several words *)

val binding_of_words_override : 'a t -> string list -> Ninja_utils.Binding.any
(** {!binding_of_words} plus the border guards (no refs, no quote characters) —
    for user-supplied override values only *)

val binding_to_words : Ninja_utils.Binding.any -> string list
(** Projection to the string-level env consumed by {!get_var}/direct exec *)

val env_of_bindings : bindings -> (string * string list) list

val cmd_concat_operand : string list -> string
(** Value for {!cat_files}: files joined for [cmd /c copy /b], each quoted. *)

val get : bindings -> 'a t -> string list
(** replaces [${xvar}] with its value, recursively *)

val expand : bindings -> string -> string
(** expands [${xvar}] references in the given string *)

val expr_elt_to_list : ?var_bindings:bindings -> Expr.elt -> string list
(** Resolves an expression element in-depth *)

val expr_elt_to_string : ?var_bindings:bindings -> Expr.elt -> string
(** Resolves an expression element in-depth, and concatenates the result with
    spaces, without escaping *)

val expr_to_list : ?var_bindings:bindings -> Expr.t -> string list
(** Resolves an expression in-depth *)

module Op : sig
  val ( ! ) : string t -> string
  (** Alias to [Var.ref]. Run-time reference to the given variable
      [!var = "${xvarname}"], for inclusion in strings *)

  val ( !! ) : 'a t -> Expr.elt
  (** Run-time reference to the given variable [!var = "${xvarname}"] as an
      expression element that will be appropriately quoted. This handles
      specific quoting rules of the [input] and [output] variables by Ninja *)
end

include module type of Op
