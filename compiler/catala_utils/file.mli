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
  ?nocolor:bool -> out_channel -> (Format.formatter -> 'a) -> 'a
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

val get_main_out_channel :
  source_file:t Global.input_src ->
  output_file:t option ->
  ?ext:string ->
  unit ->
  t option * ((out_channel -> 'a) -> 'a)
(** [get_output ~source_file ~output_file ?ext ()] returns the inferred filename
    and its corresponding [with_out_channel] function. If the [output_file] is
    equal to [Some "-"] returns a wrapper around [stdout]. *)

val get_main_out_formatter :
  source_file:t Global.input_src ->
  output_file:t option ->
  ?ext:string ->
  unit ->
  t option * ((Format.formatter -> 'a) -> 'a)
(** [get_output_format ~source_file ~output_file ?ext ()] returns the inferred
    filename and its corresponding [with_formatter_of_out_channel] function. If
    the [output_file] is equal to [Some "-"] returns a wrapper around [stdout].
*)

val with_secondary_out_channel :
  output_file:t option ->
  ext:string ->
  (t option -> Format.formatter -> 'a) ->
  'a
(** Used to open additional destination files: the main output file is
    specified, and a different extension must be provided. If the main output
    file is [None], stdout output is assumed and this passes None and a null
    formatter to the callback. [ext] specifies the extension of the output file,
    it is prefixed with a dot unless it already starts with a non-alphanumeric
    character. *)

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

val copy : src:t -> dst:t -> unit
(** File copy. Doesn't copy file attributes, only file content. The destination
    file is silently overwritten if it exists. Failures may leave a partial
    copy. [dst] can not be a directory *)

val copy_in : src:t -> dir:t -> unit
(** Same as [copy], but copies the file into the given, existing dir. *)

val copy_dir :
  ?filter:(t -> bool) -> ?newer_only:bool -> src:t -> dst:t -> unit -> unit
(** Recursively copy a directory with its contents using [copy]. [filter] is
    applied to basenames. Empty directories are not created. Does not care for
    links or attributes, or file reading errors. If [newer_only] is set to
    [true], on files for which the destination already exists, the copy is only
    done if it is older than the source *)

val remove : t -> unit
(** Recursively removes files and directories. Dangerous!

    Does not fail on already non-existent files *)

val check_directory : t -> t option
(** Checks if the given directory exists and returns it normalised (as per
    [Unix.realpath]). *)

val ensure_dir : t -> unit
(** Creates the directory (and parents recursively) if it doesn't exist already.
    Errors out if the file exists but is not a directory *)

val exists : t -> bool
(** Alias for Sys.file_exists *)

val check_file : t -> t option
(** Returns its argument if it exists and is a plain file, [None] otherwise.
    Does not do resolution like [check_directory]. *)

val check_exec : t -> t
(** Resolves a command:
    - if [t] is a plain name, resolve in PATH
    - if [t] is relative, returns its absolute path
    - fails with an error explaining that [t] was not found *)

val ( / ) : t -> t -> t
(** [Filename.concat]: Sugar to allow writing
    [File.("some" / "relative" / "path")]. As an exception, if the lhs is [.],
    returns the rhs unchanged. *)

val basename : t -> t
(** [Filename.basename], re-exported for convenience *)

val dirname : t -> t
(** [Filename.dirname], re-exported for convenience *)

val extension : t -> string
(** Like [Filename.extension], but without the leading dot (doesn't, therefore,
    differenciate between empty extension and no extension). It also considers
    catala + md extensions as a single extension, hence
    [extension "a_file.catala_en.md"] will return ["catala_en.md"] *)

val parent : t -> t
(** Similar to [dirname], except it strips the last **non-"." or ".."** element
    in the supplied file name, if it exists *)

val clean_path : t -> t
(** Rewrites a path by removing intermediate relative lookups ("." and "..").
    E.g. [../foo/./bar/../baz/] becomes [../foo/baz]. No disk lookup is made by
    this function. *)

val common_prefix : t -> t -> t
(** Returns the longuest common prefix of two paths, which are first made
    absolute *)

val make_absolute : t -> t
(** If the given file name is relative, resolve it relative to CWD and clean it
    up. Also handles Windows drive letters (e.g. turns `\foo\bar` into
    `C:\foo\bar`) *)

val remove_prefix : t -> t -> t
(** [remove_prefix prefix f] removes the [prefix] path from the beginning of [f]
    ; if [f] doesn't start with [prefix], it is returned unchanged. If [f] is
    equal to [prefix], [.] is returned. *)

val make_relative_to : dir:t -> t -> t
(** Makes [f] relative to [dir], using as many [../] as necessary. If there is
    no common prefix, returns [f] unchanged *)

val reverse_path : ?from_dir:t -> to_dir:t -> t -> t
(** If [to_dir] is a path to a given directory and [f] a path to a file as seen
    from absolute path [from_dir], [reverse_path ~from_dir ~to_dir f] is a path
    leading to [f] from [to_dir]. The results attempts to be relative to
    [to_dir]. *)

val original_cwd : t
(** The directory the command was originally run from *)

val rel_original_cwd : unit -> t
(** Same as [original_cwd], but if it is a subdirectory of the current dir, a
    relative path is returned *)

val find_in_parents : ?cwd:t -> (t -> bool) -> (t * t) option
(** Checks for the first directory matching the given predicate from [cwd]
    upwards (defaults to the current directory). Recursion stops at home.
    Returns a pair [dir, rel_path], where [dir] is the ancestor directory
    matching the predicate, and [rel_path] is a path pointing to it from the
    current dir. *)

val ( /../ ) : t -> t -> t
(** Sugar for [parent a / b] *)

val ( -.- ) : t -> string -> t
(** Extension replacement: chops the given filename extension, and replaces it
    with the given one (which shouldn't start with a dot). No dot is appended if
    the provided extension is empty. *)

val remove_extension : t -> string
(** [remove_extension filename] is equivalent to [filename -.- ""] *)

val path_to_list : t -> string option * string list
(** Returns a pair of drive letter (e.g. "c:", for windows) and a list of path
    elements. The returned path starts with an empty string for absolute
    directories ; empty elements or current-directory (".") are otherwise
    skipped in the resulting list; *)

val equal : t -> t -> bool
(** Case-insensitive string comparison (no file resolution whatsoever) *)

val compare : t -> t -> int
(** Case-insensitive string comparison (no file resolution whatsoever) *)

val format : Format.formatter -> t -> unit
(** Formats a filename in a consistent style, with double-quotes and color (when
    the output supports) *)

module Set : Set.S with type elt = t
module Map : Map.S with type key = t

val scan_tree : (t -> 'a option) -> t -> (t * t list * 'a list) Seq.t
(** Recursively scans a directory for files. Directories or files matching ".*"
    or "_*" are ignored. Unreadable files or subdirectories are ignored with a
    debug message. If [t] is a plain file, scan just that non-recursively.

    The matching results are returned grouped by directory, case-insensitively
    ordered by filename, as a list of non-empty subdirs and a list of extracted
    items. *)

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
  (** Checks if there is a matching plain file (after projection to ASCII using
      `String.to_id`, and case-insensitively) ; and returns its path with the
      correct case if so *)

  val union : t -> t -> t
  (** Merges two trees. In case of conflict, lhs entry wins *)
end
