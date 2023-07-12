(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

include module type of Stdlib.String
module Set : Set.S with type elt = string
module Map : Map.S with type key = string

(** Helper functions used for string manipulation. *)

val to_ascii : string -> string
(** Removes all non-ASCII diacritics from a string by converting them to their
    base letter in the Latin alphabet. *)

val is_uppercase_ascii : char -> bool
(** [is_uppercase c] returns if [c] is in the set ['A'...'Z']. *)

val begins_with_uppercase : string -> bool
(** [begins_with_uppercase s] returns if the first letter of [s]
    {!is_uppercase_ascii}. If [s] is empty returns false. *)

val to_snake_case : string -> string
(** Converts CamlCase into snake_case after removing Remove all diacritics on
    Latin letters. *)

val to_camel_case : string -> string
(** Converts snake_case into CamlCase after removing Remove all diacritics on
    Latin letters. *)

val remove_prefix : prefix:string -> string -> string
(** [remove_prefix ~prefix str] returns

    - if [str] starts with [prefix], a string [s] such that [prefix ^ s = str]
    - otherwise, [str] unchanged *)

val format : Format.formatter -> string -> unit

val width : string -> int
(** Returns the width of a given string in screen columns (assuming a monospace
    font). Useful for alignment. This takes unicode (except composite chars) and
    tabs into account, but not escape sequences. *)
