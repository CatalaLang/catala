(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

type t = string

(** Utility functions used for file manipulation. *)

val with_out_channel : t -> (out_channel -> 'a) -> 'a
(** Runs the given function with the provided file opened, ensuring it is
    properly closed afterwards. May raise just as [open_out]. *)

val with_in_channel : t -> (in_channel -> 'a) -> 'a
(** Runs the given function with the provided file opened, ensuring it is
    properly closed afterwards. May raise just as [open_in]. *)

(** {2 Formatter wrappers} *)

val with_formatter_of_out_channel :
  out_channel -> (Format.formatter -> 'a) -> 'a
(** [with_formatter_of_out_channel oc f] creates an flushes the formatter used
    in [f] from the given out_channel [oc]. *)

val with_formatter_of_file : t -> (Format.formatter -> 'a) -> 'a
(** [with_formatter_of_file filename f] manages the formatter created from the
    file [filename] used in [f] -- i.e. closes the corresponding out_channel and
    flushes the formatter. *)

val with_formatter_of_opt_file : t option -> (Format.formatter -> 'a) -> 'a
(** [with_formatter_of_opt_file filename_opt f] manages the formatter created
    from the file [filename_opt] if there is some (see
    {!with_formatter_of_file}), otherwise, uses the [Format.std_formatter]. *)

val get_out_channel :
  source_file:Cli.input_src ->
  output_file:t option ->
  ?ext:string ->
  unit ->
  t option * ((out_channel -> 'a) -> 'a)
(** [get_output ~source_file ~output_file ?ext ()] returns the inferred filename
    and its corresponding [with_out_channel] function. If the [output_file] is
    equal to [Some "-"] returns a wrapper around [stdout]. *)

val get_formatter_of_out_channel :
  source_file:Cli.input_src ->
  output_file:t option ->
  ?ext:string ->
  unit ->
  t option * ((Format.formatter -> 'a) -> 'a)
(** [get_output_format ~source_file ~output_file ?ext ()] returns the inferred
    filename and its corresponding [with_formatter_of_out_channel] function. If
    the [output_file] is equal to [Some "-"] returns a wrapper around [stdout]. *)

val temp_file : string -> string -> t
(** Like [Filename.temp_file], but registers the file for deletion at program
    exit unless Cli.debug_flag is set. *)

val with_temp_file : string -> string -> ?contents:string -> (t -> 'a) -> 'a
(** Creates a temp file (with prefix and suffix like [temp_file], optionally
    with the given contents, for the lifetime of the supplied function, then
    remove it unconditionally *)

val contents : t -> string
(** Reads the contents of a file as a string *)

val process_out : ?check_exit:(int -> unit) -> string -> string list -> string
(** [process_out cmd args] executes the given command with the specified
    arguments, and returns the stdout of the process as a string. [check_exit]
    is called on the return code of the sub-process, the default is to fail on
    anything but 0. *)

val check_directory : t -> t option
(** Checks if the given directory exists and returns it normalised (as per
    [Unix.realpath]). *)

val check_file : t -> t option
(** Returns its argument if it exists and is a plain file, [None] otherwise.
    Does not do resolution like [check_directory]. *)

val ( / ) : t -> t -> t
(** [Filename.concat]: Sugar to allow writing
    [File.("some" / "relative" / "path")]. As an exception, if the lhs is [.],
    returns the rhs unchanged. *)

val dirname : t -> t
(** [Filename.dirname], re-exported for convenience *)

val ( /../ ) : t -> t -> t
(** Sugar for [Filename.dirname "a" / b] *)

val ( -.- ) : t -> string -> t
(** Extension replacement: chops the given filename extension, and replaces it
    with the given one (which shouldn't contain a dot) *)

val equal : t -> t -> bool
(** Case-insensitive string comparison (no file resolution whatsoever) *)

val compare : t -> t -> int
(** Case-insensitive string comparison (no file resolution whatsoever) *)

val format : Format.formatter -> t -> unit
(** Formats a filename in a consistent style, with double-quotes and color (when
    the output supports) *)

module Set : Set.S with type elt = t
module Map : Map.S with type key = t

val scan_tree : (t -> 'a option) -> t -> 'a Seq.t
(** Recursively scans a directory for files. Directories or files matching ".*"
    or "_*" are ignored. Unreadable files or subdirectories are ignored with a
    debug message. If [t] is a plain file, scan just that non-recursively. *)

module Tree : sig
  (** A lazy tree structure mirroring the filesystem ; uses the comparison from
      File, so paths are case-insensitive. *)

  type path = t
  (** Alias for [File.t] *)

  type item = F  (** Plain file *) | D of t  (** Directory with subtree *)

  and t = (path * item) Map.t Lazy.t
  (** Contents of a directory, lazily loaded. The map keys are the basenames of
      the files and subdirectories, while the values contain the original path
      (with correct capitalisation) *)

  val empty : t

  val build : path -> t
  (** Lazily builds a [Tree.path] from the files read at [path]. The names in
      the maps are qualified (i.e. they all start with ["path/"]) *)

  val subtree : t -> path -> t
  (** Looks up a path within a lazy tree *)

  val lookup : t -> path -> path option
  (** Checks if there is a matching plain file (case-insensitively) ; and
      returns its path with the correct case if so *)

  val union : t -> t -> t
  (** Merges two trees. In case of conflict, lhs entry wins *)
end
