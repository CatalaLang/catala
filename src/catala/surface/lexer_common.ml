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

   @note -2 because [LAW_HEADING] start with at least "#" and the number of '#' remaining
   corresponds to the precedence. *)
let calc_precedence (matched_regex : string) : int = String.length matched_regex - 1

(* Gets the [LAW_HEADING] token from the current {!val: lexbuf} *)
let get_law_heading (lexbuf : lexbuf) : token =
  let extract_article_title =
    R.regexp "([#]+)\\s*([^\\|]+)(\\|([^\\|]+)|)(\\|\\s*([0-9]{4}\\-[0-9]{2}\\-[0-9]{2})|)"
  in
  let get_substring = R.get_substring (R.exec ~rex:extract_article_title (Utf8.lexeme lexbuf)) in
  let title = String.trim (get_substring 2) in
  let article_id = try Some (get_substring 4) with Not_found -> None in
  let article_expiration_date = try Some (get_substring 6) with Not_found -> None in
  let precedence = calc_precedence (get_substring 1) in
  LAW_HEADING (title, article_id, article_expiration_date, precedence)
