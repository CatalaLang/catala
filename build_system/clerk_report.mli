(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2024 Inria,
   contributors: Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** This module defines and manipulates Clerk test reports, which can be written
    by `clerk runtest` and read to provide test result summaries. This only
    concerns inline tests (```catala-test-inline blocks). *)

open Catala_utils

type test = {
  success : bool;
  command_line : string list;
  expected : Lexing.position * Lexing.position;
      (** The precise offsets of the expected result in the source file *)
  result : Lexing.position * Lexing.position;
      (** Same for the actual result in the destination file *)
}

type file = { name : File.t; successful : int; total : int; tests : test list }

val write_to : File.t -> file -> unit
val read_from : File.t -> file
val read_many : File.t -> file list
val display : build_dir:File.t -> Format.formatter -> test -> unit

val summary : build_dir:File.t -> file list -> bool
(** Displays a summary to stdout; returns true if all tests succeeded *)

val set_display_flags :
  ?files:[ `All | `Failed | `None ] ->
  ?tests:[ `All | `FailedFile | `Failed | `None ] ->
  ?diffs:bool ->
  ?use_patdiff:bool ->
  unit ->
  unit
