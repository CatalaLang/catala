(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

(** {1 Source code position} *)

(** A position in the source code is a file, as well as begin and end location of the form col:line *)
type position = {
  pos_filename: string;
  pos_loc: (Lexing.position * Lexing.position)
}

let format_position (pos: position) : string =
  let (s, e) = pos.pos_loc in
  Printf.sprintf "in file %s, from %d:%d to %d:%d"
    pos.pos_filename
    s.Lexing.pos_lnum (s.Lexing.pos_cnum - s.Lexing.pos_bol + 1)
    e.Lexing.pos_lnum (e.Lexing.pos_cnum - e.Lexing.pos_bol + 1)

(** Everything related to the source code should keep its position stored, to improve error messages *)
type 'a marked = ('a * position)


(** Placeholder position *)
let no_pos : position =
  let zero_pos =
    { Lexing.pos_fname = ""; Lexing.pos_lnum = 0; Lexing.pos_cnum = 0; Lexing.pos_bol = 0 }
  in
  {
    pos_filename = "unknown position";
    pos_loc = (zero_pos, zero_pos)
  }

let unmark ((x, _) : 'a marked) : 'a = x

let get_position ((_,x) : 'a marked) : position = x

let map_under_mark (f: 'a -> 'b) ((x, y) :'a marked) : 'b marked =
  (f x, y)

let same_pos_as (x: 'a) ((_, y) : 'b marked) : 'a marked =
  (x,y)


let unmark_option (x: 'a marked option) : 'a option = match x with
  | Some x -> Some (unmark x)
  | None -> None

module VarNameToID = Map.Make(String)
