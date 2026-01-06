(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria, contributor:
   Vincent Botbol <vincent.botbol@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** JSON encoding functions for Catala values *)

open Definitions
open Catala_runtime

val make_encoding : decl_ctx -> typ -> runtime_value Json_encoding.encoding
(** Computes a JSON encoding of a Catala type using [runtime_value] as an
    intermediate representation. *)

val scope_input_encoding :
  ScopeName.t -> decl_ctx -> typ -> runtime_value Json_encoding.encoding
(** Same as [make_encoding] but adds a title and a description to the generated
    JSON-schema expliciting that this represent a scope input structure. *)

val scope_output_encoding :
  ScopeName.t -> decl_ctx -> typ -> runtime_value Json_encoding.encoding
(** Same as [make_encoding] but adds a title and a description to the generated
    JSON-schema expliciting that this represent a scope output structure. *)

val parse_json :
  runtime_value Json_encoding.encoding -> Yojson.Safe.t -> runtime_value
(** Parse a JSON using the given encoding as validation schema. *)

val convert_to_dcalc :
  decl_ctx -> 'm mark -> typ -> runtime_value -> (dcalc, 'm) gexpr boxed
(** Conversion function from a [runtime_value] to a default calculus expression. *)

val convert_to_lcalc :
  decl_ctx -> 'm mark -> typ -> runtime_value -> (lcalc, 'm) gexpr boxed
(** Conversion function from a [runtime_value] to a lambda calculus expression. *)
