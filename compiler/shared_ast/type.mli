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
type var = Definitions.typ_var

val format : Format.formatter -> t -> unit

module Map : Catala_utils.Map.S with type key = t

val equal : t -> t -> bool
val equal_list : t list -> t list -> bool
val compare : t -> t -> int

val map : (t -> t Bindlib.box) -> t -> t Bindlib.box
(** Shallow mapping on types *)

val hash : strip:Uid.Path.t -> t -> Hash.t
(** The [strip] argument strips the given leading path components in included
    identifiers before hashing *)

val unifiable : t -> t -> bool
(** Similar to [equal], but allows TVar holes; variables are assumed to be
    unifiable with anything that doesn't contain them *)

val unifiable_list : t list -> t list -> bool

val arrow_return : t -> t
(** Returns the last member in nested [TArrow] types *)

val has_arrow : Definitions.decl_ctx -> t -> bool
(** Fails (with [Invalid_argument]) on TAny and TClosureEnv *)

val unquantify : t -> t
(** Removes the outermost quantifiers from the given type, if any. The returned
    type is guaranteed to not have the form [TAny _] and may contain free
    variables *)

val any : Pos.t -> t
(** Returns a quantified type variable ([TAny ('a)]) (the returned type is
    closed thus safe to box) *)

val new_var : Pos.t -> t
(** Returns a fresh type variable, without a quantifier. The variable is not
    boxed, if binding is needed use [Var.fresh] and [Bindlib.box_var] directly
    instead*)

(** Handling of variables *)

module Var : sig
  type t = var

  val fresh : Pos.t -> t

  module Set : Set.S with type elt = t
  module Map : Catala_utils.Map.S with type key = t
end

val free_vars : t -> Var.Set.t
