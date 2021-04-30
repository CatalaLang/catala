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

open Tokens
open Sedlexing
module R = Re.Pcre

(* Calculates the precedence according a {!val: matched_regex} of the form : '[#]+'.

   @note -2 because both [LAW_ARTICLE] and [LAW_HEADING] start with at least "##" and the number of
   '#' remaining corresponds to the precedence. *)
let calc_precedence (matched_regex : string) : int = String.length matched_regex - 2

(* Gets the [LAW_HEADING] token from the current {!val: lexbuf} *)
let get_law_heading (lexbuf : lexbuf) : token =
  let extract_code_title = R.regexp "([#]+)\\s*([^#\n]+)\n" in
  let get_match = R.get_substring (R.exec ~rex:extract_code_title (Utf8.lexeme lexbuf)) in
  let get_new_lines = R.regexp "\n" in
  let new_lines_count =
    try Array.length (R.extract ~rex:get_new_lines (Utf8.lexeme lexbuf)) with Not_found -> 0
  in

  (* the -1 is here to compensate for Sedlex's automatic newline detection around token *)
  for _i = 1 to new_lines_count - 1 do
    new_line lexbuf
  done;
  let law_title = get_match 2 in
  let precedence = calc_precedence (get_match 1) in
  LAW_HEADING (law_title, precedence)
