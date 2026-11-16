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

type attr = ..

type t = {
  file : string;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
  attr : attr list;
}
(* Lines and columns are 1-based, columns are counted in code points and
   [end_col] is exclusive. *)

let get_start_line (pos : t) : int = pos.start_line
let get_start_column (pos : t) : int = pos.start_col
let get_end_line (pos : t) : int = pos.end_line
let get_end_column (pos : t) : int = pos.end_col
let get_file (pos : t) : string = pos.file

type attr += Law_pos of string list

let from_info
    (file : string)
    (start_line : int)
    (start_col : int)
    (end_line : int)
    (end_col : int) : t =
  { file; start_line; start_col; end_line; end_col; attr = [] }

let from_lpos ((s, e) : Lexing.position * Lexing.position) : t =
  let open Lexing in
  from_info s.pos_fname s.pos_lnum
    (s.pos_cnum - s.pos_bol + 1)
    e.pos_lnum
    (e.pos_cnum - e.pos_bol + 1)

let compare_start p1 p2 =
  match Int.compare p1.start_line p2.start_line with
  | 0 -> Int.compare p1.start_col p2.start_col
  | n -> n

let compare_end p1 p2 =
  match Int.compare p1.end_line p2.end_line with
  | 0 -> Int.compare p1.end_col p2.end_col
  | n -> n

let compare p1 p2 =
  let ( |>| ) n f = match n with 0 -> f p1 p2 | n -> n in
  String.compare p1.file p2.file |>| compare_start |>| compare_end

let equal p1 p2 = compare p1 p2 = 0
let attrs t = t.attr
let set_attrs t attr = { t with attr }
let add_attr t attr = { t with attr = attr :: t.attr }

let add_attrs t attrs =
  { t with attr = List.fold_left (fun xs x -> x :: xs) t.attr attrs }

let filter_attr t f = { t with attr = List.filter f t.attr }
let get_attrs t f = List.filter_map f t.attr

let get_attr t f =
  match get_attrs t f with
  | [] -> None
  | [a] -> Some a
  | _ :: _ :: _ -> invalid_arg "Pos.get_attr: multiple matching attributes"

let has_attr t a = List.mem a t.attr

let take_attr t f =
  let rec aux acc = function
    | [] -> None, List.rev acc
    | a :: r -> (
      match f a with
      | None -> aux (a :: acc) r
      | some -> some, List.rev_append acc r)
  in
  let r, attr = aux [] t.attr in
  r, { t with attr }

let join_attr a1 a2 =
  if List.exists (function Law_pos _ -> true | _ -> false) a1 then
    a1 @ List.filter (function Law_pos _ -> false | _ -> true) a2
  else a1 @ a2

let join (p1 : t) (p2 : t) : t =
  if p1.file = "" then p2
  else if p2.file = "" then p1
  else if p1.file <> p2.file then invalid_arg "Pos.join"
  else
    (* We must keep latest start for join_attr which guarantees
    that the Law of the earliest position is selected*)
    let earliest_start, latest_start =
      if compare_start p1 p2 <= 0 then p1, p2 else p2, p1
    in
    let latest_end = if compare_end p1 p2 <= 0 then p2 else p1 in
    {
      file = p1.file;
      start_line = earliest_start.start_line;
      start_col = earliest_start.start_col;
      end_line = latest_end.end_line;
      end_col = latest_end.end_col;
      attr = join_attr earliest_start.attr latest_start.attr;
    }

let from_file name : t = from_info name 0 0 0 0

let overwrite_law_info (pos : t) (law_pos : string list) : t =
  {
    pos with
    attr =
      Law_pos law_pos
      :: List.filter (function Law_pos _ -> false | _ -> true) pos.attr;
  }

let get_law_info (pos : t) : string list =
  Option.value ~default:[]
  @@ List.find_map (function Law_pos l -> Some l | _ -> None) pos.attr

let to_string (pos : t) : string =
  Printf.sprintf "in file %s, from %d:%d to %d:%d" pos.file pos.start_line
    pos.start_col pos.end_line pos.end_col

let range_to_string pos =
  if pos.start_line = pos.end_line then
    (* range is on a single line *)
    if pos.start_col = 1 then
      (* range starts at the begining of the line *)
      Printf.sprintf "%d" pos.start_line
    else if pos.start_col = pos.end_col then
      (* range is a single column wide *)
      Printf.sprintf "%d.%d" pos.start_line pos.end_col
    else
      (* range is on a single line but non degenerate *)
      Printf.sprintf "%d.%d-%d" pos.start_line pos.start_col pos.end_col
  else if pos.start_col = 1 then
    (* range is several lines, but starts on the first column of the first line *)
    Printf.sprintf "%d-%d" pos.start_line pos.end_line
  else
    (* Several lines: "3.5-7.2" *)
    Printf.sprintf "%d.%d-%d.%d" pos.start_line pos.start_col pos.end_line
      pos.end_col

let to_string_short (pos : t) : string =
  Printf.sprintf "%s:%s" pos.file (range_to_string pos)

let to_string_shorter (pos : t) : string =
  let f = pos.file in
  let f =
    if Filename.extension f = ".md" then
      Filename.(remove_extension (remove_extension (basename f)))
    else Filename.(remove_extension (basename f))
  in
  Printf.sprintf "%s:%s" f (range_to_string pos)

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

let format_loc_text_parts
    ?(pp_file = fun ppf pos -> Format.pp_print_string ppf (to_string_short pos))
    (pos : t) =
  let filename = get_file pos in
  let pr_head ppf =
    Format.fprintf ppf "@{<blue>─➤ @{<bold>%a:@}@}@," pp_file pos
  in
  let pr_context =
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
            n >= sline - include_extra_count && n <= eline + include_extra_count
          then (n, line) :: get_lines (n + 1)
          else []
        | None -> []
      in
      let pos_lines = get_lines 1 in
      let nspaces = int_of_float (log10 (float_of_int eline)) + 1 in
      (match ic with None -> () | Some ic -> close_in ic);
      let line_matched_substrings =
        try
          List.filter_map
            (fun (line_no, line) ->
              let line_indent = indent_number line in
              let match_start_index =
                utf8_byte_index line
                  (if line_no = sline then get_start_column pos - 1
                   else line_indent)
              in
              if line_no = eline && get_end_column pos <= 1 then None
              else
                let match_end_index =
                  if line_no = eline then
                    utf8_byte_index line (get_end_column pos - 1)
                  else String.length line
                in
                Some
                  ( line_no,
                    line,
                    String.sub line 0 match_start_index,
                    String.sub line match_start_index
                      (max 0 (match_end_index - match_start_index)) ))
            pos_lines
        with Invalid_argument _ ->
          (* Index out of bounds, can happen if the file was changed since
             initially read *)
          []
      in
      let print_matched_line
          ppf
          (line_no, line, unmatched_prefix, matched_substring) =
        let match_start_col = String.width unmatched_prefix in
        let match_num_cols = String.width matched_substring in
        Format.fprintf ppf "@{<blue>%*d │@} %a" nspaces line_no
          (fun ppf -> Format.pp_print_as ppf (String.width line))
          (Re.replace_string ~by:"        " Re.(compile (char '\t')) line);
        Format.pp_print_cut ppf ();
        if line_no >= sline && line_no <= eline then
          Format.fprintf ppf "@{<blue>%*s │@} %*s@{<bold;red>%t@}" nspaces ""
            match_start_col ""
            (pad_fmt match_num_cols "‾")
      in
      if line_matched_substrings <> [] then (fun ppf ->
        Format.fprintf ppf "@{<blue> %*s│@}@," nspaces "";
        Format.pp_print_list print_matched_line ppf line_matched_substrings)
      else ignore
    with Sys_error _ -> ignore
  in
  let pr_legal =
    let legal_pos_lines =
      List.rev_map
        (fun s ->
          Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\n\\s*")
            ~subst:(fun _ -> " ")
            s)
        (get_law_info pos)
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
    match legal_pos_lines with
    | [] -> None
    | fst :: rest ->
      Some
        (fun ppf ->
          Format.fprintf ppf "@{<blue>%s@}" fst;
          pp_legal 0 rest ppf)
  in
  pr_head, pr_context, pr_legal

let format_loc_text ?pp_file () ppf t =
  if get_file t = "" then (
    if Global.options.debug then
      Format.pp_print_string ppf " (no position information)")
  else
    let pr_head, pr_context, pr_legal = format_loc_text_parts ?pp_file t in
    Format.pp_open_vbox ppf 0;
    pr_head ppf;
    pr_context ppf;
    Option.iter
      (fun f ->
        Format.pp_print_cut ppf ();
        f ppf)
      pr_legal;
    Format.pp_close_box ppf ()

let void : t = from_info "" 0 1 0 1

module Ord = struct
  type nonrec t = t

  let compare = compare
  let format ppf t = Format.pp_print_string ppf (to_string_short t)
end

module Set = Set.Make (Ord)
module Map = Map.Make (Ord)
