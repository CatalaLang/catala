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

module type Info = sig
  type info

  val to_string : info -> string
  val format : Format.formatter -> info -> unit
  val equal : info -> info -> bool
  val compare : info -> info -> int
end

module type Id = sig
  type t
  type info

  val fresh : info -> t
  val get_info : t -> info
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val format_t : Format.formatter -> t -> unit
  val hash : t -> int

  module Set : Set.S with type elt = t
  module SetLabels : MoreLabels.Set.S with type elt = t and type t = Set.t
  module Map : Map.S with type key = t
  module MapLabels : MoreLabels.Map.S with type key = t and type 'a t = 'a Map.t
end

module Make (X : Info) () : Id with type info = X.info = struct
  module Ordering = struct
    type t = { id : int; info : X.info }

    let compare (x : t) (y : t) : int = compare x.id y.id
    let equal x y = Int.equal x.id y.id
  end

  include Ordering

  type info = X.info

  let counter = ref 0

  let fresh (info : X.info) : t =
    incr counter;
    { id = !counter; info }

  let get_info (uid : t) : X.info = uid.info
  let format_t (fmt : Format.formatter) (x : t) : unit = X.format fmt x.info
  let hash (x : t) : int = x.id

  module Set = Set.Make (Ordering)
  module Map = Map.Make (Ordering)
  module SetLabels = MoreLabels.Set.Make (Ordering)
  module MapLabels = MoreLabels.Map.Make (Ordering)
end

module MarkedString = struct
  type info = string Mark.pos

  let to_string (s, _) = s
  let format fmt i = Format.pp_print_string fmt (to_string i)
  let equal = Mark.equal String.equal
  let compare = Mark.compare String.compare
end

module Gen () = Make (MarkedString) ()
