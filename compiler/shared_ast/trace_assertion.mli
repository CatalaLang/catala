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

(** Checks the values declared by the [#[trace.assert]] attributes against the
    trace produced by the interpreter, and renders the mismatches. *)

type asserted_trace_variable = string

val asserted_trace_variable_to_string : asserted_trace_variable -> string

type value = Catala_runtime.Value.t

val value_to_string : value -> string

type trace_assertions

val empty : trace_assertions

val add_asserted_trace_variable :
  string -> value option -> trace_assertions -> trace_assertions

type failed_trace_assertions = {
  name : asserted_trace_variable;
      (** The variable the attributes were attached to *)
  expected : value;
      (** Its expected value, normalised as the trace writes it *)
  current_value : value option;
      (** What the trace holds at the declared path, [None] when the path leads
          nowhere or the element carries no value *)
  pos : Catala_runtime.code_location option;
      (** Where the value comes from: the position embedded in an unwrapped
          default value, else the trace element's own; [None] when the path
          leads nowhere *)
}

val check :
  asserted_trace_variables:trace_assertions ->
  tested_scope:string ->
  Catala_runtime.trace ->
  failed_trace_assertions list
(** [check ~asserted_trace_variables ~tested_scope trace] returns one entry per
    variable whose value does not match [trace]. Variables without a path are
    reported in a single warning and left unchecked. *)

val trace_assertion_to_json : failed_trace_assertions -> Yojson.t
val display : Format.formatter -> failed_trace_assertions -> unit
val report_display : Format.formatter -> failed_trace_assertions -> unit
