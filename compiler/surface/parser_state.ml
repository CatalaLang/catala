(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria,
   contributors: Louis Gesbert <louis.gesbert@inria.fr>

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

type t = {
  current_file : File.t;
  mutable current_heading : Ast.law_heading list;
}

let state : t list ref = ref []

let get_state () =
  match !state with
  | [] -> failwith "Running the parser must be done within Parser.with_state"
  | st :: _ -> st

let with_state f lexbuf =
  let current_file =
    (fst (Sedlexing.lexing_positions lexbuf)).Lexing.pos_fname
  in
  let cur_state = { current_file; current_heading = [] } in
  state := cur_state :: !state;
  let ret = f lexbuf in
  match !state with
  | new_state :: r ->
    assert (new_state.current_file = current_file);
    state := r;
    ret
  | [] -> assert false

let new_heading heading lpos =
  let state = get_state () in
  let title, id, is_archive, precedence = heading in
  let upper_headings =
    List.filter
      (fun hd -> hd.Ast.law_heading_precedence < precedence)
      state.current_heading
  in
  let pos =
    Pos.overwrite_law_info (Pos.from_lpos lpos)
      (List.map (fun h -> Mark.remove h.Ast.law_heading_name) upper_headings)
  in
  let heading =
    {
      Ast.law_heading_name = title, pos;
      law_heading_id = id;
      law_heading_is_archive = is_archive;
      law_heading_precedence = precedence;
    }
  in
  state.current_heading <- heading :: upper_headings;
  heading

let get_current_heading () =
  List.map
    (fun h -> Mark.remove h.Ast.law_heading_name)
    (get_state ()).current_heading
