(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

(** {1 Source code position} *)

(** A position in the source code is a file, as well as begin and end location of the form col:line *)
type position = {
  pos_filename: string;
  pos_loc: (Lexing.position * Lexing.position)
}

let format_position fmt (pos: position) =
  let (s, e) = pos.pos_loc in
  Format.fprintf fmt "in file %s, from %d:%d to %d:%d"
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
