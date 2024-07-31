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

let to_string (pos : t) : string =
  let s, e = pos.code_pos in
  Printf.sprintf "in file %s, from %d:%d to %d:%d" s.Lexing.pos_fname
    s.Lexing.pos_lnum
    (s.Lexing.pos_cnum - s.Lexing.pos_bol)
    e.Lexing.pos_lnum
    (e.Lexing.pos_cnum - e.Lexing.pos_bol)

let to_string_short (pos : t) : string =
  let s, e = pos.code_pos in
  Printf.sprintf "%s:%d.%d-%d.%d" s.Lexing.pos_fname s.Lexing.pos_lnum
    (s.Lexing.pos_cnum - s.Lexing.pos_bol + 1)
    e.Lexing.pos_lnum
    (e.Lexing.pos_cnum - e.Lexing.pos_bol + 1)

let indent_number (s : string) : int =
  try
    let rec aux (i : int) = if s.[i] = ' ' then aux (i + 1) else i in
    aux 0
  with Invalid_argument _ -> String.length s

let utf8_byte_index s ui0 =
  let rec aux bi ui =
    if ui >= ui0 then bi
    else
      aux (bi + Uchar.utf_decode_length (String.get_utf_8_uchar s bi)) (ui + 1)
  in
  aux 0 0

let rec pad_fmt n s ppf =
  if n > 0 then (
    Format.pp_print_as ppf 1 s;
    pad_fmt (n - 1) s ppf)

let format_loc_text_parts (pos : t) =
  let filename = get_file pos in
  if filename = "" then
    ( (fun ppf -> Format.pp_print_string ppf "No position information"),
      ignore,
      None )
  else
    let pr_head ppf =
      Format.fprintf ppf "@{<blue>─➤ @{<bold>%s:@}@}@," (to_string_short pos)
    in
    let pr_context, pr_legal =
      try
        let sline = get_start_line pos in
        let eline = get_end_line pos in
        let ic, input_line_opt =
          let from_contents =
            match Global.options.input_src with
            | Contents (str, uri) when uri = filename -> Some str
            | _ -> None
          in
          match from_contents with
          | Some str ->
            let line_index = ref 0 in
            let lines = String.split_on_char '\n' str in
            let input_line_opt () : string option =
              match List.nth_opt lines !line_index with
              | Some l ->
                line_index := !line_index + 1;
                Some l
              | None -> None
            in
            None, input_line_opt
          | None -> (
            try
              let ic = open_in filename in
              let input_line_opt () : string option =
                try Some (input_line ic) with End_of_file -> None
              in
              Some ic, input_line_opt
            with Sys_error _ -> None, fun () -> None)
        in
        let include_extra_count = 0 in
        let rec get_lines (n : int) : (int * string) list =
          match input_line_opt () with
          | Some line ->
            if n < sline - include_extra_count then get_lines (n + 1)
            else if
              n >= sline - include_extra_count
              && n <= eline + include_extra_count
            then (n, line) :: get_lines (n + 1)
            else []
          | None -> []
        in
        let pos_lines = get_lines 1 in
        let nspaces = int_of_float (log10 (float_of_int eline)) + 1 in
        (match ic with None -> () | Some ic -> close_in ic);
        let print_matched_line ppf ((line_no, line) : int * string) =
          let line_indent = indent_number line in
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
          let match_start_col = String.width unmatched_prefix in
          let match_num_cols = String.width matched_substring in
          Format.fprintf ppf "@{<blue>%*d │@} %a" nspaces line_no
            (fun ppf -> Format.pp_print_as ppf (String.width line))
            line;
          Format.pp_print_cut ppf ();
          if line_no >= sline && line_no <= eline then
            Format.fprintf ppf "@{<blue>%*s │@} %*s@{<bold;red>%t@}" nspaces ""
              match_start_col ""
              (pad_fmt match_num_cols "‾")
        in
        let pr_context ppf =
          Format.fprintf ppf "@{<blue> %*s│@}@," nspaces "";
          Format.pp_print_list print_matched_line ppf pos_lines
        in
        let legal_pos_lines =
          List.rev_map
            (fun s ->
              Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\n\\s*")
                ~subst:(fun _ -> " ")
                s)
            pos.law_pos
        in
        let rec pp_legal nspaces leg ppf =
          match leg with
          | [last] ->
            Format.fprintf ppf "@,@{<blue>%*s@<2>%s %s@}" nspaces "" "└─" last
          | l :: lines ->
            Format.fprintf ppf "@,@{<blue>%*s@<2>%s %s@}" nspaces "" "└┬" l;
            pp_legal (nspaces + 1) lines ppf
          | [] -> ()
        in
        let pr_law =
          match legal_pos_lines with
          | [] -> None
          | fst :: rest ->
            Some
              (fun ppf ->
                Format.fprintf ppf "@{<blue>%s@}" fst;
                pp_legal 0 rest ppf)
        in
        pr_context, pr_law
      with Sys_error _ -> ignore, None
    in
    pr_head, pr_context, pr_legal

let format_loc_text ppf t =
  let pr_head, pr_context, pr_legal = format_loc_text_parts t in
  Format.pp_open_vbox ppf 0;
  pr_head ppf;
  pr_context ppf;
  Option.iter
    (fun f ->
      Format.pp_print_cut ppf ();
      f ppf)
    pr_legal;
  Format.pp_close_box ppf ()

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
