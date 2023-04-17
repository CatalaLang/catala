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

type t = { code_pos : Lexing.position * Lexing.position; law_pos : string list }

let from_lpos (p : Lexing.position * Lexing.position) : t =
  { code_pos = p; law_pos = [] }

let lex_pos_compare lp1 lp2 =
  match String.compare lp1.Lexing.pos_fname lp2.Lexing.pos_fname with
  | 0 -> Int.compare lp1.Lexing.pos_cnum lp2.Lexing.pos_cnum
  | n -> n

let join (p1 : t) (p2 : t) : t =
  if (fst p1.code_pos).Lexing.pos_fname <> (fst p2.code_pos).Lexing.pos_fname
  then invalid_arg "Pos.join";
  let beg1, end1 = p1.code_pos in
  let beg2, end2 = p2.code_pos in
  {
    code_pos =
      ( (if lex_pos_compare beg1 beg2 <= 0 then beg1 else beg2),
        if lex_pos_compare end1 end2 <= 0 then end2 else end1 );
    law_pos =
      (if lex_pos_compare beg1 beg2 <= 0 then p1.law_pos else p2.law_pos);
  }

let from_info
    (file : string)
    (sline : int)
    (scol : int)
    (eline : int)
    (ecol : int) : t =
  let spos =
    {
      Lexing.pos_fname = file;
      Lexing.pos_lnum = sline;
      Lexing.pos_cnum = scol;
      Lexing.pos_bol = 1;
    }
  in
  let epos =
    {
      Lexing.pos_fname = file;
      Lexing.pos_lnum = eline;
      Lexing.pos_cnum = ecol;
      Lexing.pos_bol = 1;
    }
  in
  { code_pos = spos, epos; law_pos = [] }

let overwrite_law_info (pos : t) (law_pos : string list) : t =
  { pos with law_pos }

let get_law_info (pos : t) : string list = pos.law_pos

let get_start_line (pos : t) : int =
  let s, _ = pos.code_pos in
  s.Lexing.pos_lnum

let get_start_column (pos : t) : int =
  let s, _ = pos.code_pos in
  s.Lexing.pos_cnum - s.Lexing.pos_bol + 1

let get_end_line (pos : t) : int =
  let _, e = pos.code_pos in
  e.Lexing.pos_lnum

let get_end_column (pos : t) : int =
  let _, e = pos.code_pos in
  e.Lexing.pos_cnum - e.Lexing.pos_bol + 1

let get_file (pos : t) : string = (fst pos.code_pos).Lexing.pos_fname

type input_file = FileName of string | Contents of string

let to_string (pos : t) : string =
  let s, e = pos.code_pos in
  Printf.sprintf "in file %s, from %d:%d to %d:%d" s.Lexing.pos_fname
    s.Lexing.pos_lnum
    (s.Lexing.pos_cnum - s.Lexing.pos_bol)
    e.Lexing.pos_lnum
    (e.Lexing.pos_cnum - e.Lexing.pos_bol)

let to_string_short (pos : t) : string =
  let s, e = pos.code_pos in
  Printf.sprintf "%s:%d.%d-%d.%d:" s.Lexing.pos_fname s.Lexing.pos_lnum
    (s.Lexing.pos_cnum - s.Lexing.pos_bol + 1)
    e.Lexing.pos_lnum
    (e.Lexing.pos_cnum - e.Lexing.pos_bol + 1)

let indent_number (s : string) : int =
  try
    let rec aux (i : int) = if s.[i] = ' ' then aux (i + 1) else i in
    aux 0
  with Invalid_argument _ -> String.length s

let string_repeat n s =
  let slen = String.length s in
  let buf = Bytes.create (n * slen) in
  for i = 0 to n - 1 do
    Bytes.blit_string s 0 buf (i * slen) slen
  done;
  Bytes.to_string buf

(* Note: this should do, but remains incorrect for combined unicode characters
   that display as one (e.g. `e` + postfix `'`). We should switch to Uuseg at
   some poing *)
let string_columns s =
  let len = String.length s in
  let rec aux ncols i =
    if i >= len then ncols
    else if s.[i] = '\t' then aux (ncols + 8) (i + 1)
    else
      aux (ncols + 1) (i + Uchar.utf_decode_length (String.get_utf_8_uchar s i))
  in
  aux 0 0

let utf8_byte_index s ui0 =
  let rec aux bi ui =
    if ui >= ui0 then bi
    else
      aux (bi + Uchar.utf_decode_length (String.get_utf_8_uchar s bi)) (ui + 1)
  in
  aux 0 0

let retrieve_loc_text (pos : t) : string =
  try
    let filename = get_file pos in
    let blue_style = [ANSITerminal.Bold; ANSITerminal.blue] in
    if filename = "" then "No position information"
    else
      let sline = get_start_line pos in
      let eline = get_end_line pos in
      let oc, input_line_opt =
        if filename = "stdin" then
          let line_index = ref 0 in
          let lines = String.split_on_char '\n' !Cli.contents in
          let input_line_opt () : string option =
            match List.nth_opt lines !line_index with
            | Some l ->
              line_index := !line_index + 1;
              Some l
            | None -> None
          in
          None, input_line_opt
        else
          let oc = open_in filename in
          let input_line_opt () : string option =
            try Some (input_line oc) with End_of_file -> None
          in
          Some oc, input_line_opt
      in
      let print_matched_line (line : string) (line_no : int) : string =
        let line_indent = indent_number line in
        let error_indicator_style = [ANSITerminal.red; ANSITerminal.Bold] in
        let match_start_index =
          utf8_byte_index line
            (if line_no = sline then get_start_column pos - 1 else line_indent)
        in
        let match_end_index =
          if line_no = eline then utf8_byte_index line (get_end_column pos - 1)
          else String.length line
        in
        let unmatched_prefix = String.sub line 0 match_start_index in
        let matched_substring =
          String.sub line match_start_index
            (max 0 (match_end_index - match_start_index))
        in
        let match_start_col = string_columns unmatched_prefix in
        let match_num_cols = string_columns matched_substring in
        String.concat ""
          (line
          :: "\n"
          ::
          (if line_no >= sline && line_no <= eline then
           [
             string_repeat match_start_col " ";
             Cli.with_style error_indicator_style "%s"
               (string_repeat match_num_cols "‾");
           ]
          else []))
      in
      let include_extra_count = 0 in
      let rec get_lines (n : int) : string list =
        match input_line_opt () with
        | Some line ->
          if n < sline - include_extra_count then get_lines (n + 1)
          else if
            n >= sline - include_extra_count && n <= eline + include_extra_count
          then print_matched_line line n :: get_lines (n + 1)
          else []
        | None -> []
      in
      let pos_lines = get_lines 1 in
      let spaces = int_of_float (log10 (float_of_int eline)) + 1 in
      let legal_pos_lines =
        List.rev
          (List.map
             (fun s ->
               Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\n\\s*")
                 ~subst:(fun _ -> " ")
                 s)
             pos.law_pos)
      in
      (match oc with None -> () | Some oc -> close_in oc);
      let buf = Buffer.create 73 in
      Buffer.add_string buf
        (Cli.with_style blue_style "┌─⯈ %s" (to_string_short pos));
      Buffer.add_char buf '\n';
      (* should be outside of [Cli.with_style] *)
      Buffer.add_string buf
        (Cli.with_style blue_style "└%s┐" (string_repeat spaces "─"));
      Buffer.add_char buf '\n';
      Buffer.add_string buf
        (Cli.add_prefix_to_each_line (String.concat "\n" pos_lines) (fun i ->
             let cur_line = sline - include_extra_count + i in
             if
               cur_line >= sline
               && cur_line <= sline + (2 * (eline - sline))
               && cur_line mod 2 = sline mod 2
             then
               Cli.with_style blue_style "%*d │" spaces
                 (sline + ((cur_line - sline) / 2))
             else if cur_line >= sline - include_extra_count && cur_line < sline
             then Cli.with_style blue_style "%*d │" spaces (cur_line + 1)
             else if
               cur_line
               <= sline + (2 * (eline - sline)) + 1 + include_extra_count
               && cur_line > sline + (2 * (eline - sline)) + 1
             then
               Cli.with_style blue_style "%*d │" spaces
                 (cur_line - (eline - sline + 1))
             else Cli.with_style blue_style "%*s │" spaces ""));
      Buffer.add_char buf '\n';
      let () =
        match legal_pos_lines with
        | [] -> ()
        | _ ->
          let last = List.length legal_pos_lines - 1 in
          Buffer.add_string buf
            (Cli.add_prefix_to_each_line
               (String.concat "\n"
                  (List.map
                     (fun l -> Cli.with_style blue_style "%s" l)
                     legal_pos_lines))
               (fun i ->
                 if i = last then
                   Cli.with_style blue_style "%*s└─" (spaces + i + 1) ""
                 else Cli.with_style blue_style "%*s└┬" (spaces + i + 1) ""))
      in
      Buffer.contents buf
  with Sys_error _ -> "Location:" ^ to_string pos

let no_pos : t =
  let zero_pos =
    {
      Lexing.pos_fname = "";
      Lexing.pos_lnum = 0;
      Lexing.pos_cnum = 0;
      Lexing.pos_bol = 0;
    }
  in
  { code_pos = zero_pos, zero_pos; law_pos = [] }
