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

(** Source code position *)

type t
(** A position in the source code is a file, as well as begin and end location
    of the form col:line, and optionally, attributes *)

val compare : t -> t -> int
val equal : t -> t -> bool

type attr = ..
type attr += Law_pos of string list

(**{2 Constructor and getters}*)

val from_lpos : Lexing.position * Lexing.position -> t
val from_info : string -> int -> int -> int -> int -> t
val overwrite_law_info : t -> string list -> t
val get_law_info : t -> string list
val get_start_line : t -> int
val get_start_column : t -> int
val get_end_line : t -> int
val get_end_column : t -> int
val get_file : t -> string
val attrs : t -> attr list
val set_attrs : t -> attr list -> t
val add_attr : t -> attr -> t
val add_attrs : t -> attr list -> t

val get_attr : t -> (attr -> 'a option) -> 'a option
(** Raises [Invalid_argument] if the attribute appears multiple times *)

val has_attr : t -> attr -> bool

val take_attr : t -> (attr -> 'a option) -> 'a option * t
(** Removes the found attribute if it is present *)

val get_attrs : t -> (attr -> 'a option) -> 'a list

val join : t -> t -> t
(** Returns the smallest range including both supplied ranges. Attributes are
    merged ; law info is taken from the earliest position.

    Raises [Invalid_argument] if they don't belong to the same file. *)

module Map : Map.S with type key = t

(**{2 Formatters}*)

val to_string : t -> string
(** Formats a position like this:

    {v in file <file>, from <start_line>:<start_col> to <end_line>:<end_col> v} *)

val to_string_short : t -> string
(** Formats a position like this:

    {v <file>;<start_line>:<start_col>--<end_line>:<end_col> v}

    This function is compliant with the
    {{:https://www.gnu.org/prep/standards/standards.html#Errors} GNU coding
      standards}. *)

val to_string_shorter : t -> string
(** Like [to_string_short], but skips directory names and extension *)

val format_loc_text : Format.formatter -> t -> unit
(** Open the file corresponding to the position and retrieves the text concerned
    by the position *)

val format_loc_text_parts :
  t ->
  (Format.formatter -> unit)
  * (Format.formatter -> unit)
  * (Format.formatter -> unit) option
(** Like [format_loc_text], but returns the printing functions in 3 separate
    parts: the file name header, the line context, and the law headers *)

val void : t
(** Placeholder position *)

(**/**)

val pad_fmt : int -> string -> Format.formatter -> unit
(** Exported as [Message.pad] *)
