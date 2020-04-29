(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Error formatting and helper functions *)

(**{1 Parsing}*)

exception ParsingError of string

exception LexingError of string

exception WeavingError of string

let retrieve_loc_text (pos : Pos.t) : string =
  let filename = Pos.get_file pos in
  let sline = Pos.get_start_line pos in
  let eline = Pos.get_end_line pos in
  let oc = open_in filename in
  let input_line_opt () : string option = try Some (input_line oc) with End_of_file -> None in
  let print_matched_line (line : string) (line_no : int) : string =
    line ^ "\n"
    ^
    if line_no = sline && line_no = eline then
      ANSITerminal.sprintf [ ANSITerminal.red ] "%*s"
        (Pos.get_end_column pos - 1)
        (String.make (Pos.get_end_column pos - Pos.get_start_column pos) '^')
    else if line_no = sline && line_no <> eline then
      ANSITerminal.sprintf [ ANSITerminal.red ] "%*s"
        (String.length line - 1)
        (String.make (String.length line - Pos.get_start_column pos) '^')
    else if line_no <> sline && line_no <> eline then
      ANSITerminal.sprintf [ ANSITerminal.red ] "%s" (String.make (String.length line) '^')
    else
      (* if line_no<> sline && line_no = eline then *)
      ANSITerminal.sprintf [ ANSITerminal.red ] "%*s"
        (Pos.get_end_column pos - 1)
        (String.make (Pos.get_end_column pos) '^')
  in
  let rec get_lines (n : int) : string list =
    match input_line_opt () with
    | Some line ->
        if n < sline then get_lines (n + 1)
        else if n >= sline && n <= eline then print_matched_line line n :: get_lines (n + 1)
        else []
    | None -> []
  in
  let pos_lines = List.rev (get_lines 1) in
  close_in oc;
  Printf.sprintf "<%s>\n%s" filename
    (Cli.add_prefix_to_each_line
       (Printf.sprintf "\n%s\n" (String.concat "\n" pos_lines))
       (fun i -> Printf.sprintf "%*d | " (int_of_float (log (float_of_int eline))) (sline + i - 1)))

let parser_error (loc : Lexing.position * Lexing.position) (token : string) (msg : string) =
  raise
    (ParsingError
       (Printf.sprintf "Syntax error at token \"%s\" %s\n%s\n%s" token (Pos.to_string loc)
          (retrieve_loc_text loc) msg))

let lexer_error (loc : Lexing.position * Lexing.position) (msg : string) =
  raise (LexingError (Printf.sprintf "Parsing error %s on token \"%s\"" (Pos.to_string loc) msg))

let weaving_error (msg : string) = raise (WeavingError (Printf.sprintf "Weaving error: %s" msg))
