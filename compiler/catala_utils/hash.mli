(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Hashes for the identification of modules.

    In contrast with OCaml's basic `Hashtbl.hash`, they process the full depth
    of terms. Any meaningful interface change in a module should only be in hash
    collision with a 1/2^30 probability. *)

type t = private int
(** Native Hasthbl.hash hashes, value is truncated to 30 bits whatever the
    architecture (positive 31-bit integers) *)

type full
(** A "full" hash includes the Catala version and compilation flags, alongside
    the module interface *)

val raw : 'a -> t
(** [Hashtbl.hash]. Do not use on deep types (it has a bounded depth), use
    specific hashing functions. *)

module Op : sig
  val ( ! ) : 'a -> t
  (** Shortcut to [raw]. Same warning: use with an explicit type annotation
      [!(foo: string)] to ensure it's not called on types that are recursive or
      include annotations.

      Hint: we use [!`Foo] as a fancy way to generate constants for
      discriminating constructions *)

  val ( % ) : t -> t -> t
  (** Safe combination of two hashes (non commutative or associative, etc.) *)
end

val option : ('a -> t) -> 'a option -> t
val list : ('a -> t) -> 'a list -> t

val map :
  (('k -> 'v -> t -> t) -> 'map -> t -> t) ->
  ('k -> t) ->
  ('v -> t) ->
  'map ->
  t
(** [map fold_f key_hash_f value_hash_f map] computes the hash of a map. The
    first argument is expected to be a [Foo.Map.fold] function. The result is
    independent of the ordering of the map. *)

val finalise :
  t ->
  avoid_exceptions:bool ->
  closure_conversion:bool ->
  monomorphize_types:bool ->
  full
(** Turns a raw interface hash into a full hash, ready for printing *)

val to_string : full -> string
val format : Format.formatter -> full -> unit

val of_string : string -> full
(** @raise Failure *)
