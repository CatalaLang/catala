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
open Definitions

type 'a t = 'a gtyp
type 'a var = 'a naked_gtyp Bindlib.var

val format : Format.formatter -> 'a t -> unit
val equal : ?unionfind:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val equal_list : ?unionfind:('a -> 'a -> bool) -> 'a t list -> 'a t list -> bool
val compare : nil t -> nil t -> int

val map : ('a gtyp -> 'b gtyp Bindlib.box) -> 'a gtyp -> 'b gtyp Bindlib.box
(** Shallow mapping on types *)

val shallow_fold : ('b t -> 'a -> 'a) -> 'b t -> 'a -> 'a

val hash : strip:Uid.Path.t -> nil t -> Hash.t
(** The [strip] argument strips the given leading path components in included
    identifiers before hashing *)

val unifiable : nil t -> nil t -> bool
(** Similar to [equal], but allows TVar holes; variables are assumed to be
    unifiable with anything that doesn't contain them *)

val unifiable_list : nil t list -> nil t list -> bool

val arrow_return : nil t -> nil t
(** Returns the last member in nested [TArrow] types *)

val has_arrow : Definitions.decl_ctx -> nil t -> bool
(** Fails (with [Invalid_argument]) on TVar and TClosureEnv *)

val any : Pos.t -> 'a t
(** Returns a quantified type variable ([TAny ('a)]) (the returned type is
    closed thus safe to box) *)

val new_var : Pos.t -> 'a t
(** Returns a fresh type variable, without a quantifier. The variable is not
    boxed, if binding is needed use [Var.fresh] and [Bindlib.box_var] directly
    instead*)

(** Handling of variables *)

module type VarSig = sig
  type t

  val fresh : unit -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val format : Format.formatter -> t -> unit

  val translate : 'a var -> 'b var

  module Set : Set.S with type elt = t
  module Map : Catala_utils.Map.S with type key = t
end

module Var: VarSig with type t = naked_typ Bindlib.var

val free_vars : nil t -> Var.Set.t

val rebox : 'a t -> 'a t Bindlib.box

val unquantify : 'a t -> ' a t
(** Removes the outermost quantifiers from the given type, if any. The returned
    type is guaranteed to not have the form [TAny _] and may contain free
    variables *)

module MakeVar(V: sig type t end): VarSig with type t = V.t naked_gtyp Bindlib.var
