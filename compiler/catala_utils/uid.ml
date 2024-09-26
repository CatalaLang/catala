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
  val hash : info -> Hash.t
end

module type Id = sig
  type t
  type info

  val fresh : info -> t
  val get_info : t -> info
  val map_info : (info -> info) -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val format : Format.formatter -> t -> unit
  val to_string : t -> string
  val id : t -> int
  val hash : t -> Hash.t

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
      (* Format.pp_print_int ppf t.id; (* uncomment for precise uid debug *) *)
      Format.pp_close_stag ppf ()
  end

  include Ordering

  type info = X.info

  let counter = ref 0

  let fresh (info : X.info) : t =
    incr counter;
    { id = !counter; info }

  let get_info (uid : t) : X.info = uid.info
  let map_info f { id; info } = { id; info = f info }
  let id (x : t) : int = x.id
  let to_string t = X.to_string t.info
  let hash t = X.hash t.info

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
  let hash = Mark.hash String.hash
end

module Gen (S : Style) () = Make (MarkedString) (S) ()

(* - Modules, paths and qualified idents - *)

module Module =
  Gen
    (struct
      let style = Ocolor_types.(Fg (C4 blue))
    end)
    ()

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

  let strip prefix p0 =
    let rec aux prefix p =
      match prefix, p with
      | pfx1 :: pfx, p1 :: p -> if Module.equal pfx1 p1 then aux pfx p else p0
      | [], p -> p
      | _ -> p0
    in
    aux prefix p0
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

  let hash (p, i) =
    let open Hash.Op in
    Hash.list Module.hash p % MarkedString.hash i
end

module type Qualified = sig
  include Id with type info = Path.t * MarkedString.info

  val fresh : Path.t -> MarkedString.info -> t
  val path : t -> Path.t
  val get_info : t -> MarkedString.info
  val base : t -> string

  val hash : strip:Path.t -> t -> Hash.t
  (** [strip] strips that prefix from the start of the path before hashing *)
end

module Gen_qualified (S : Style) () : Qualified = struct
  include Make (QualifiedMarkedString) (S) ()

  let fresh path t = fresh (path, t)

  let hash ~strip t =
    let p, i = get_info t in
    QualifiedMarkedString.hash (Path.strip strip p, i)

  let path t = fst (get_info t)
  let get_info t = snd (get_info t)
  let base t = Mark.remove (get_info t)
end
