(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Definitions

(** {1 Variables and their collections} *)

(** This module provides types and helpers for Bindlib variables on the [gexpr]
    type *)

type 'e t = ('a, 't) naked_gexpr Bindlib.var constraint 'e = ('a, 't) gexpr
type 'e vars = ('a, 't) naked_gexpr Bindlib.mvar constraint 'e = ('a, 't) gexpr

val make : string -> 'e t
val compare : 'e t -> 'e t -> int
val equal : 'e t -> 'e t -> bool
val hash : 'e t -> int

val translate : 'e1 t -> 'e2 t
(** Needed when converting from one AST type to another. See the note of caution
    on [Bindlib.copy_var]. *)

type 'e var = 'e t
(** Alias to allow referring to the type in the submodules *)

(** Wrapper over [Set.S] but with a type variable for the AST type parameters.
    Extend as needed *)
module Set : sig
  type 'e t

  val empty : 'e t
  val singleton : 'e var -> 'e t
  val add : 'e var -> 'e t -> 'e t
  val remove : 'e var -> 'e t -> 'e t
  val union : 'e t -> 'e t -> 'e t
  val mem : 'e var -> 'e t -> bool
  val of_list : 'e var list -> 'e t
  val elements : 'e t -> 'e var list
  val diff : 'e t -> 'e t -> 'e t
  val iter : ('e var -> unit) -> 'e t -> unit
end

(** Wrapper over [Map.S] but with a type variable for the AST type parameters.
    Extend as needed *)
module Map : sig
  type ('e, 'x) t
  type k0

  exception Not_found of k0

  val empty : ('e, 'x) t
  val singleton : 'e var -> 'x -> ('e, 'x) t
  val add : 'e var -> 'x -> ('e, 'x) t -> ('e, 'x) t
  val update : 'e var -> ('x option -> 'x option) -> ('e, 'x) t -> ('e, 'x) t
  val find : 'e var -> ('e, 'x) t -> 'x
  val find_opt : 'e var -> ('e, 'x) t -> 'x option
  val bindings : ('e, 'x) t -> ('e var * 'x) list
  val mem : 'e var -> ('e, 'x) t -> bool

  val union :
    ('e var -> 'x -> 'x -> 'x option) -> ('e, 'x) t -> ('e, 'x) t -> ('e, 'x) t

  val fold : ('e var -> 'x -> 'acc -> 'acc) -> ('e, 'x) t -> 'acc -> 'acc
  val keys : ('e, 'x) t -> 'e var list
  val values : ('e, 'x) t -> 'x list
end
