(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
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

open Catala_utils

type t = Definitions.typ
type var = Definitions.naked_typ Bindlib.var

val format : Format.formatter -> t -> unit

module Map : Catala_utils.Map.S with type key = t

val equal : t -> t -> bool
val equal_list : t list -> t list -> bool
val compare : t -> t -> int

val map : (t -> t Bindlib.box) -> t -> t Bindlib.box
(** Shallow mapping on types *)

val shallow_fold : (t -> 'a -> 'a) -> t -> 'a -> 'a

val hash : strip:Uid.Path.t -> t -> Hash.t
(** The [strip] argument strips the given leading path components in included
    identifiers before hashing *)

val unifiable : t -> t -> bool

val unifiable_list : t list -> t list -> bool
(** Similar to [equal], but allows TForAll holes *)

val arrow_return : t -> t
(** Returns the last member in nested [TArrow] types *)

val has_arrow : Definitions.decl_ctx -> t -> bool
(** Fails (with [Invalid_argument]) on TForAll and TClosureEnv *)

(** Handling of variables *)

module Var : sig
  include Catala_utils.Map.OrderedType with type t = var
  module Map : Catala_utils.Map.S with type key = t
  module Set : Set.S with type elt = t
  module Hashtbl : Hashtbl.S with type key = t

  val fresh : unit -> t
  val equal : t -> t -> bool
  val hash : t -> int
end

val free_vars : t -> Var.Set.t

val free_vars_pos : t -> Pos.t Var.Map.t
(** Only the first position where the variable appeared is kept *)

val rebox : t -> t Bindlib.box

val unquantify : t -> t
(** Removes the outermost quantifiers from the given type, if any. The returned
    type is guaranteed to not have the form [TForAll _] and may contain free
    variables *)

val unbind : t -> Var.t list * t
(** Recursively unbinds nested quantifiers and merges their variables (this is
    similar to [unquantify], but also returns the bound variables) *)

val forall : Var.t list -> t Bindlib.box -> Pos.t -> t
(** The opposite of [unbind]: constructs a [TForAll] quantified type *)

val fresh_var : Pos.t -> t

val any : Pos.t -> t
(** deprecated, TODO replace with fresh_var *)

val universal : Pos.t -> t
val is_universal : t -> bool
