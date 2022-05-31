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
    (s.Lexing.pos_cnum - s.Lexing.pos_bol + 1)
    e.Lexing.pos_lnum
    (e.Lexing.pos_cnum - e.Lexing.pos_bol + 1)

let to_string_short (pos : t) : string =
  let s, e = pos.code_pos in
  Printf.sprintf "%s;%d:%d--%d:%d" s.Lexing.pos_fname s.Lexing.pos_lnum
    (s.Lexing.pos_cnum - s.Lexing.pos_bol + 1)
    e.Lexing.pos_lnum
    (e.Lexing.pos_cnum - e.Lexing.pos_bol + 1)

let indent_number (s : string) : int =
  try
    let rec aux (i : int) = if s.[i] = ' ' then aux (i + 1) else i in
    aux 0
  with Invalid_argument _ -> String.length s

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
        line
        ^
        if line_no >= sline && line_no <= eline then
          "\n"
          ^
          if line_no = sline && line_no = eline then
            Cli.with_style error_indicator_style "%*s"
              (get_end_column pos - 1)
              (String.make
                 (max (get_end_column pos - get_start_column pos) 0)
                 '^')
          else if line_no = sline && line_no <> eline then
            Cli.with_style error_indicator_style "%*s"
              (String.length line - 1)
              (String.make
                 (max (String.length line - get_start_column pos) 0)
                 '^')
          else if line_no <> sline && line_no <> eline then
            Cli.with_style error_indicator_style "%*s%s" line_indent ""
              (String.make (max (String.length line - line_indent) 0) '^')
          else if line_no <> sline && line_no = eline then
            Cli.with_style error_indicator_style "%*s%*s" line_indent ""
              (get_end_column pos - 1 - line_indent)
              (String.make (max (get_end_column pos - line_indent) 0) '^')
          else assert false (* should not happen *)
        else ""
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
      Cli.with_style blue_style "%*s--> %s\n%s\n%s" spaces "" filename
        (Cli.add_prefix_to_each_line
           (Printf.sprintf "\n%s" (String.concat "\n" pos_lines))
           (fun i ->
             let cur_line = sline - include_extra_count + i - 1 in
             if
               cur_line >= sline
               && cur_line <= sline + (2 * (eline - sline))
               && cur_line mod 2 = sline mod 2
             then
               Cli.with_style blue_style "%*d | " spaces
                 (sline + ((cur_line - sline) / 2))
             else if cur_line >= sline - include_extra_count && cur_line < sline
             then Cli.with_style blue_style "%*d | " spaces cur_line
             else if
               cur_line
               <= sline + (2 * (eline - sline)) + 1 + include_extra_count
               && cur_line > sline + (2 * (eline - sline)) + 1
             then
               Cli.with_style blue_style "%*d | " spaces
                 (cur_line - (eline - sline + 1))
             else Cli.with_style blue_style "%*s | " spaces ""))
        (Cli.add_prefix_to_each_line
           (Printf.sprintf "%s"
              (String.concat "\n"
                 (List.map
                    (fun l -> Cli.with_style blue_style "%s" l)
                    legal_pos_lines)))
           (fun i ->
             if i = 0 then
               Cli.with_style blue_style "%*s + " (spaces + (2 * i)) ""
             else Cli.with_style blue_style "%*s+-+ " (spaces + (2 * i) - 1) ""))
  with Sys_error _ -> "Location:" ^ to_string pos

type 'a marked = 'a * t

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

let mark pos e : 'a marked = e, pos
let unmark ((x, _) : 'a marked) : 'a = x
let get_position ((_, x) : 'a marked) : t = x
let map_under_mark (f : 'a -> 'b) ((x, y) : 'a marked) : 'b marked = f x, y
let same_pos_as (x : 'a) ((_, y) : 'b marked) : 'a marked = x, y

let compare_marked
    (cmp : 'a -> 'a -> int)
    ((x, _) : 'a marked)
    ((y, _) : 'a marked) : int =
  cmp x y

let unmark_option (x : 'a marked option) : 'a option =
  match x with Some x -> Some (unmark x) | None -> None

class ['self] marked_map =
  object (_self : 'self)
    constraint
    'self = < visit_marked :
                'a. ('env -> 'a -> 'a) -> 'env -> 'a marked -> 'a marked
            ; .. >

    method visit_marked
        : 'a. ('env -> 'a -> 'a) -> 'env -> 'a marked -> 'a marked =
      fun f env x -> same_pos_as (f env (unmark x)) x
  end

class ['self] marked_iter =
  object (_self : 'self)
    constraint
    'self = < visit_marked :
                'a. ('env -> 'a -> unit) -> 'env -> 'a marked -> unit
            ; .. >

    method visit_marked : 'a. ('env -> 'a -> unit) -> 'env -> 'a marked -> unit
        =
      fun f env x -> f env (unmark x)
  end
