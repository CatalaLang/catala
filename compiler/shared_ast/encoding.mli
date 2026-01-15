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

(** {1 Runtime values to JSON encodings correspondence}

    {v
    - Unit: {} (empty JSON object)

    - Bool: true | false (JSON boolean)

    - Money: 123 (JSON integer) | 123.45 (JSON number) | "123" (JSON string)
      Note:
        - integer and string representations map to monetary units and not cents
        - floats are truncated to the second decimal

    - Integer: 123 (JSON integer) | "123" (JSON string)

    - Decimal: 123 (JSON integer) | 123.45 (JSON number) | "1/3" (JSON string)
      Note: we rely on [Q.of_string] for decoding strings

    - Date: "1970-01-31" (JSON string)
      Note: we rely on [Dates_calc.date_of_string]

    - Duration: "1 years", "2 months" or "12 days" (JSON string)

    - Enum:
      - Unit constructors: "A" (JSON string)
      - Non-unit constructors: {"B": <json value>} (JSON object)

    - Struct: { "x": <json value>, "y": <json value>, ...} (JSON object)

    - Array: [ <json value>, <json value>, ...] (JSON array)

    - Tuple: [ <json value>, <json value>, ...] (JSON array)
    v} *)

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
(** Conversion function from a [runtime_value] to a default calculus expression.
*)

val convert_to_lcalc :
  decl_ctx -> 'm mark -> typ -> runtime_value -> (lcalc, 'm) gexpr boxed
(** Conversion function from a [runtime_value] to a lambda calculus expression.
*)

val convert_from_gexpr : decl_ctx -> (_, 'm) gexpr -> runtime_value
(** Conversion function from a generic expression to a runtime value. This
    functions expects the expression to be composed of encodable-only nodes
    (e.g., no lambdas, closures, etc.) *)
