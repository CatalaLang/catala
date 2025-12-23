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

open Definitions

val make_encoding :
  decl_ctx -> typ -> Catala_runtime.runtime_value Json_encoding.encoding

val parse_json :
  Catala_runtime.runtime_value Json_encoding.encoding ->
  Yojson.Safe.t ->
  Catala_runtime.runtime_value

val convert_to_dcalc :
  decl_ctx ->
  'm mark ->
  typ ->
  Catala_runtime.runtime_value ->
  (dcalc, 'm) gexpr boxed

val convert_to_lcalc :
  decl_ctx ->
  'm mark ->
  typ ->
  Catala_runtime.runtime_value ->
  (lcalc, 'm) gexpr boxed
