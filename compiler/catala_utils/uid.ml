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
  val format : Format.formatter -> t -> unit
  val hash : t -> int

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

module type Style = sig
  val style : Ocolor_types.style
end

module Make (X : Info) (S : Style) () : Id with type info = X.info = struct
  module Ordering = struct
    type t = { id : int; info : X.info }

    let compare (x : t) (y : t) : int = Int.compare x.id y.id
    let equal x y = Int.equal x.id y.id

    let format ppf t =
      Format.pp_open_stag ppf (Ocolor_format.Ocolor_style_tag S.style);
      X.format ppf t.info;
      Format.pp_close_stag ppf ()
  end

  include Ordering

  type info = X.info

  let counter = ref 0

  let fresh (info : X.info) : t =
    incr counter;
    { id = !counter; info }

  let get_info (uid : t) : X.info = uid.info
  let hash (x : t) : int = x.id

  module Set = Set.Make (Ordering)
  module Map = Map.Make (Ordering)
end

(* - Raw idents - *)

module MarkedString = struct
  type info = string Mark.pos

  let to_string (s, _) = s
  let format fmt i = String.format fmt (to_string i)
  let equal = Mark.equal String.equal
  let compare = Mark.compare String.compare
end

module Gen (S : Style) () = Make (MarkedString) (S) ()

(* - Modules, paths and qualified idents - *)

module Module = struct
  module Ordering = struct
    type t = string Mark.pos

    let equal = Mark.equal String.equal
    let compare = Mark.compare String.compare
    let format ppf m = Format.fprintf ppf "@{<blue>%s@}" (Mark.remove m)
  end

  include Ordering

  let to_string m = Mark.remove m
  let of_string m = m
  let pos m = Mark.get m

  module Set = Set.Make (Ordering)
  module Map = Map.Make (Ordering)
end
(* TODO: should probably be turned into an uid once we implement module import
   directives; that will incur an additional resolution work on all paths though
   ([module Module = Gen ()]) *)

module Path = struct
  type t = Module.t list

  let format ppf p =
    Format.pp_print_list
      ~pp_sep:(fun _ () -> ())
      (fun ppf m -> Format.fprintf ppf "%a@{<cyan>.@}" Module.format m)
      ppf p

  let to_string p = String.concat "." (List.map Module.to_string p)
  let equal = List.equal Module.equal
  let compare = List.compare Module.compare
end

module QualifiedMarkedString = struct
  type info = Path.t * MarkedString.info

  let to_string (p, i) =
    Format.asprintf "%a%a" Path.format p MarkedString.format i

  let format fmt (p, i) =
    Path.format fmt p;
    MarkedString.format fmt i

  let equal (p1, i1) (p2, i2) = Path.equal p1 p2 && MarkedString.equal i1 i2

  let compare (p1, i1) (p2, i2) =
    match Path.compare p1 p2 with 0 -> MarkedString.compare i1 i2 | n -> n
end

module Gen_qualified (S : Style) () = struct
  include Make (QualifiedMarkedString) (S) ()

  let fresh path t = fresh (path, t)
  let path t = fst (get_info t)
  let get_info t = snd (get_info t)
end
