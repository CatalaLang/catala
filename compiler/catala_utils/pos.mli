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

(** Source code positions.

    Positions are attached to virtually every node of the various ASTs of the
    compiler (see {!Mark}), and are used for error reporting, for linking
    generated code back to the original source, and for carrying extra metadata
    in the form of {!attr}ibutes. *)

type t
(** A position in the source code: a file name, a start and end location (ech
    consisting in a line and column), and a list of attributes ({!attr}). The
    law headings surrounding the position are stored as a {!Law_pos} attribute.

    Lines and columns are 1-based. Columns are counted in Unicode code points.
*)

val compare : t -> t -> int
(** Orders positions by file name, then by start (line, then column), then by
    end. Attributes are ignored. *)

val equal : t -> t -> bool
(** Equality of the source ranges, consistent with {!compare}. Attributes are
    ignored. *)

type attr = ..
(** Extensible type of attributes that can be attached to positions. Other
    modules extend it to carry their own metadata along with a position (e.g.
    user-written attributes in [Shared_ast.Definitions], or plugins). Attributes
    are compared with structural equality by {!has_attr}. *)

type attr +=
  | Law_pos of string list
        (** The law headings enclosing the position, innermost first. Use
            {!get_law_info} and {!overwrite_law_info} to access it. *)

(**{2 Constructor and getters}*)

val from_lpos : Lexing.position * Lexing.position -> t
(** Builds a position from the start and end positions given by the lexer, with
    no attributes. *)

val from_info : string -> int -> int -> int -> int -> t
(** [from_info file start_line start_col end_line end_col] builds a position
    with no attributes. Lines and columns are 1-based. *)

val from_file : string -> t
(** A position pointing to a whole file, without specific range (lines and
    columns are set to [0]). *)

val overwrite_law_info : t -> string list -> t
(** Replaces the law headings of the position (innermost first) *)

val get_law_info : t -> string list
(** The law headings of the position, innermost first. Returns [[]] if there are
    none. *)

val get_start_line : t -> int
val get_start_column : t -> int
val get_end_line : t -> int

val get_end_column : t -> int
(** The end column is exclusive: it points to the first character after the
    range. *)

val get_file : t -> string
(** The name of the file of the position, as passed to the compiler. It is [""]
    for {!void}. *)

(**{3 Attributes} *)

val attrs : t -> attr list
(** All attributes attached to the position, including {!Law_pos} *)

val set_attrs : t -> attr list -> t
(** Replaces all the attributes of the position *)

val add_attr : t -> attr -> t
(** Adds an attribute to the position. Duplicates are not checked. *)

val add_attrs : t -> attr list -> t

val filter_attr : t -> (attr -> bool) -> t
(** [filter_attr pos f] keeps only the attributes of [pos] that satisfy [f] *)

val get_attr : t -> (attr -> 'a option) -> 'a option
(** [get_attr pos f] returns the result of [f] on the only attribute for which
    it returns [Some _], or [None] if there is none. Raises [Invalid_argument]
    if the attribute appears multiple times. *)

val has_attr : t -> attr -> bool
(** Whether the position has the given attribute (using structural equality) *)

val take_attr : t -> (attr -> 'a option) -> 'a option * t
(** [take_attr pos f] finds the first attribute of [pos] for which [f] returns
    [Some x], and returns [Some x] along with [pos] without that attribute. If
    there is none, returns [None] and [pos] unchanged. Other matching attributes
    are left in place. *)

val get_attrs : t -> (attr -> 'a option) -> 'a list
(** Returns the results of [f] on all attributes for which it returns [Some _]
*)

(**{3 Combining positions} *)

val join : t -> t -> t
(** Returns the smallest range including both supplied ranges. Attributes are
    merged ; law info is taken from the earliest position.

    Void positions (filename = "") are ignored. Raises [Invalid_argument] if
    they don't belong to the same file. *)

module Set : Set.S with type elt = t
(** Sets of positions, ordered as by {!compare} *)

module Map : Map.S with type key = t
(** Maps with positions as keys, ordered as by {!compare} *)

(**{2 Formatters}*)

val to_string : t -> string
(** Formats a position like this:

    {v in file <file>, from <start_line>:<start_col> to <end_line>:<end_col> v}
*)

val to_string_short : t -> string
(** Formats a position in a compact form, as [<file>:<range>], where [<range>]
    is the first of these that applies:
    - [<line>] if the range is on a single line and starts at column 1 (the end
      column is not printed)
    - [<line>.<col>] for an empty range
    - [<line>.<start_col>-<end_col>] for a range within a single line
    - [<start_line>-<end_line>] if the range spans several lines and starts at
      column 1 (the end column is not printed)
    - [<start_line>.<start_col>-<end_line>.<end_col>] otherwise *)

val to_string_shorter : t -> string
(** Like [to_string_short], but skips directory names and extension (both
    extensions for files ending in [.md], e.g. [foo.catala_en.md] becomes
    [foo]). Directories are recognised using the separator of the system the
    compiler runs on. *)

val format_loc_text :
  ?pp_file:(Format.formatter -> t -> unit) ->
  unit ->
  Format.formatter ->
  t ->
  unit
(** Open the file corresponding to the position and retrieves the text concerned
    by the position. The output contains a header with the file name (printed
    with [pp_file], which defaults to {!to_string_short}), the source lines with
    the range underlined, and the enclosing law headings.

    If the input of the compiler was given as a string (see
    [Global.options.input_src]) and matches the file of the position, the text
    is retrieved from there instead. If the file can't be read, only the header
    and law headings are printed. Prints nothing for {!void} (except in debug
    mode). *)

val format_loc_text_parts :
  ?pp_file:(Format.formatter -> t -> unit) ->
  t ->
  (Format.formatter -> unit)
  * (Format.formatter -> unit)
  * (Format.formatter -> unit) option
(** Like [format_loc_text], but returns the printing functions in 3 separate
    parts: the file name header, the line context, and the law headings ([None]
    if there are none). *)

val void : t
(** Placeholder position, used when no actual position is available. Its file
    name is [""]; it is ignored by {!join}. *)

(**/**)

val pad_fmt : int -> string -> Format.formatter -> unit
(** Exported as [Message.pad] *)
