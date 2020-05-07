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

let indent_number (s : string) : int =
  try
    let rec aux (i : int) = if s.[i] = ' ' then aux (i + 1) else i in
    aux 0
  with Invalid_argument _ -> String.length s

let retrieve_loc_text (pos : Pos.t) : string =
  let filename = Pos.get_file pos in
  let sline = Pos.get_start_line pos in
  let eline = Pos.get_end_line pos in
  let oc = open_in filename in
  let input_line_opt () : string option = try Some (input_line oc) with End_of_file -> None in
  let print_matched_line (line : string) (line_no : int) : string =
    let line_indent = indent_number line in
    let error_indicator_style = [ ANSITerminal.red; ANSITerminal.Bold ] in
    line
    ^
    if line_no >= sline && line_no <= eline then
      "\n"
      ^
      if line_no = sline && line_no = eline then
        ANSITerminal.sprintf error_indicator_style "%*s"
          (Pos.get_end_column pos - 1)
          (String.make (Pos.get_end_column pos - Pos.get_start_column pos) '^')
      else if line_no = sline && line_no <> eline then
        ANSITerminal.sprintf error_indicator_style "%*s"
          (String.length line - 1)
          (String.make (String.length line - Pos.get_start_column pos) '^')
      else if line_no <> sline && line_no <> eline then
        ANSITerminal.sprintf error_indicator_style "%*s%s" line_indent ""
          (String.make (String.length line - line_indent) '^')
      else if line_no <> sline && line_no = eline then
        ANSITerminal.sprintf error_indicator_style "%*s%*s" line_indent ""
          (Pos.get_end_column pos - 1 - line_indent)
          (String.make (Pos.get_end_column pos - line_indent) '^')
      else assert false (* should not happen *)
    else ""
  in
  let include_extra_count = 1 in
  let rec get_lines (n : int) : string list =
    match input_line_opt () with
    | Some line ->
        if n < sline - include_extra_count then get_lines (n + 1)
        else if n >= sline - include_extra_count && n <= eline + include_extra_count then
          print_matched_line line n :: get_lines (n + 1)
        else []
    | None -> []
  in
  let pos_lines = get_lines 1 in
  let spaces = int_of_float (log (float_of_int eline)) in
  close_in oc;
  Printf.sprintf "%*s--> %s\n%s" spaces "" filename
    (Cli.add_prefix_to_each_line
       (Printf.sprintf "\n%s\n" (String.concat "\n" pos_lines))
       (fun i ->
         let cur_line = sline - include_extra_count + i - 1 in
         if
           cur_line >= sline
           && cur_line <= sline + (2 * (eline - sline))
           && cur_line mod 2 = sline mod 2
         then Printf.sprintf "%*d | " spaces (sline + ((cur_line - sline) / 2))
         else if cur_line >= sline - include_extra_count && cur_line < sline then
           Printf.sprintf "%*d | " spaces cur_line
         else if
           cur_line <= sline + (2 * (eline - sline)) + 1 + include_extra_count
           && cur_line > sline + (2 * (eline - sline)) + 1
         then Printf.sprintf "%*d | " spaces (cur_line - (eline - sline + 1))
         else Printf.sprintf "%*s | " spaces ""))

let parser_error (loc : Lexing.position * Lexing.position) (token : string) (msg : string) =
  raise
    (ParsingError
       (Printf.sprintf "Syntax error at token \"%s\" %s\n%s\n%s" token (Pos.to_string loc)
          (retrieve_loc_text loc) msg))

let lexer_error (loc : Lexing.position * Lexing.position) (msg : string) =
  raise (LexingError (Printf.sprintf "Parsing error %s on token \"%s\"" (Pos.to_string loc) msg))

let weaving_error (msg : string) = raise (WeavingError (Printf.sprintf "Weaving error: %s" msg))
