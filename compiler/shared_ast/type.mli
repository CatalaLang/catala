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

val format : Format.formatter -> t -> unit

module Map : Catala_utils.Map.S with type key = t

val equal : t -> t -> bool
val equal_list : t list -> t list -> bool
val compare : t -> t -> int

val map : (t -> t) -> t -> t
(** Shallow mapping on types *)

val hash : strip:Uid.Path.t -> t -> Hash.t
(** The [strip] argument strips the given leading path components in included
    identifiers before hashing *)

val unifiable : t -> t -> bool

val unifiable_list : t list -> t list -> bool
(** Similar to [equal], but allows TAny holes *)

val arrow_return : t -> t
(** Returns the last member in nested [TArrow] types *)
