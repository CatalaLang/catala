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

type asserted_trace_variable

val asserted_trace_variable_to_string : asserted_trace_variable -> string

module M : Map.S with type key = asserted_trace_variable

type value

val value_to_string : value -> string

type trace_assertions = value M.t

val add_asserted_trace_variable :
  string -> string -> trace_assertions -> trace_assertions

type trace_assertion = {
  name : asserted_trace_variable;
      (** The variable the attributes were attached to *)
  expected : value;
      (** Its expected value, normalised as the trace writes it *)
  current_value : value option;
      (** What the trace holds at the declared path, [None] when the path leads
          nowhere or the element carries no value *)
}

val normalize_value : value -> value
(** Re-renders a value written in the surface syntax ("100,25 €", "$100.25",
    "2024-1-3"...) the way the trace encoder does, so that both can be compared
    as plain strings. *)

val read_trace : File.t -> Yojson.Safe.t option
(** Parses a trace file, failing with an error message rather than an exception.
    Convenience for the callers that do have a file; the trace may also be built
    in memory. *)

val check :
  asserted_trace_variables:trace_assertions ->
  tested_scope:string ->
  Yojson.Safe.t option ->
  trace_assertion list
(** [check ~asserted_trace_variables ~tested_scope trace] returns one entry per
    variable whose value does not match [trace]. Variables without a path are
    reported in a single warning and left unchecked. *)

val trace_assertion_to_json : trace_assertion -> Yojson.t
val display : Format.formatter -> trace_assertion -> unit
