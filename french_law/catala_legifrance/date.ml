(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, contributor:
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

open Catala_utils

(** Helper functions to interact with {!Unix.tm} dates *)

type date_format = DDMMYYYY | ISO

(** Parses a date formatted as [DD/MM/YYYY] or [YYYY-MM-DD] into an {!Unix.tm}*)
let parse_expiration_date (date_format : date_format) (expiration_date : string)
    : Unix.tm =
  try
    let extract_article_title =
      Re.Pcre.regexp
        (match date_format with
        | DDMMYYYY -> "([0-9]{2})\\/([0-9]{2})\\/([0-9]{4})"
        | ISO -> "([0-9]{4})\\-([0-9]{2})\\-([0-9]{2})")
    in
    let get_substring =
      Re.Pcre.get_substring
        (Re.Pcre.exec ~rex:extract_article_title expiration_date)
    in
    snd
      (Unix.mktime
         {
           Unix.tm_mday =
             int_of_string
               (get_substring
                  (match date_format with DDMMYYYY -> 1 | ISO -> 3));
           Unix.tm_mon = int_of_string (get_substring 2);
           Unix.tm_year =
             int_of_string
               (get_substring
                  (match date_format with DDMMYYYY -> 3 | ISO -> 1))
             - 1900;
           Unix.tm_sec = 0;
           Unix.tm_min = 0;
           Unix.tm_hour = 0;
           Unix.tm_wday = 0;
           Unix.tm_yday = 0;
           Unix.tm_isdst = false;
         })
  with _ ->
    Messages.raise_error "Error while parsing expiration date argument (%s)"
      expiration_date

(** Prints an [Unix.tm] under the ISO formatting [YYYY-MM-DD] *)
let print_tm (d : Unix.tm) : string =
  if d.Unix.tm_year + 1900 = 2999 then "undefined date"
  else
    Printf.sprintf "%d-%02d-%02d" (1900 + d.Unix.tm_year) (1 + d.Unix.tm_mon)
      d.Unix.tm_mday

(** Returns true if [d] is set in the year [2999] *)
let is_infinity (d : Unix.tm) : bool = d.Unix.tm_year + 1900 = 2999

(** [date_compare d1 d2] compares the timestamps of [d1] and [d2]*)
let date_compare (d1 : Unix.tm) (d2 : Unix.tm) : int =
  int_of_float (fst (Unix.mktime d1)) - int_of_float (fst (Unix.mktime d2))
