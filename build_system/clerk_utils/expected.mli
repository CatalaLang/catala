(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020 Inria,
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

(** Checks the values declared by the [#[testcase.variable]] attributes against
    the trace produced by the interpreter, and renders the mismatches. *)

open Catala_utils

type expected = {
  name : string;  (** The variable the attributes were attached to *)
  expected : string;
      (** Its expected value, normalised as the trace writes it *)
  current_value : string option;
      (** What the trace holds at the declared path, [None] when the path leads
          nowhere or the element carries no value *)
}

val normalize_value : string -> string
(** Re-renders a value written in the surface syntax ("100,25 €", "$100.25",
    "2024-1-3"...) the way the trace encoder does, so that both can be compared
    as plain strings. *)

val read_trace : File.t -> Yojson.Safe.t
(** Parses a trace file, failing with an error message rather than an exception.
    Convenience for the callers that do have a file; the trace may also be built
    in memory. *)

val check_expected :
  expected:Scan.expected_variable Scan.M.t ->
  tested_scope:string ->
  Yojson.Safe.t ->
  expected list
(** [check_expected ~expected ~tested_scope trace] returns one entry per
    variable whose value does not match [trace], following the path given by its
    [#[testcase.variable.path]] attribute. Variables without a path are reported
    in a single warning and left unchecked. *)

val display_expected : Format.formatter -> expected -> unit
