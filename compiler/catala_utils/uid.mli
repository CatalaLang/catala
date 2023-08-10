(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Global identifiers factories using a generative functor *)

(** The information carried in global identifiers *)
module type Info = sig
  type info

  val to_string : info -> string
  val format : Format.formatter -> info -> unit

  val equal : info -> info -> bool
  (** Equality disregards position *)

  val compare : info -> info -> int
  (** Comparison disregards position *)
end

module MarkedString : Info with type info = string Mark.pos
(** The only kind of information carried in Catala identifiers is the original
    string of the identifier annotated with the position where it is declared or
    used. *)

(** Identifiers have abstract types, but are comparable so they can be used as
    keys in maps or sets. Their underlying information can be retrieved at any
    time. *)
module type Id = sig
  type t
  type info

  val fresh : info -> t
  val get_info : t -> info
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val format : Format.formatter -> t -> unit
  val hash : t -> int

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

(** This is the generative functor that ensures that two modules resulting from
    two different calls to [Make] will be viewed as different types [t] by the
    OCaml typechecker. Prevents mixing up different sorts of identifiers. *)
module Make (X : Info) () : Id with type info = X.info

module Gen () : Id with type info = MarkedString.info
(** Shortcut for creating a kind of uids over marked strings *)
