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

(** Conversion of surface expressions denoting a value (e.g. the expected values
    of [#[trace.assert]] attributes) into runtime values. *)

val value_of_literal : Ast.literal -> Catala_runtime.Value.t option
(** Returns [None] on literals that have no exact runtime value, e.g. decimal
    durations or invalid dates. *)

val value_of_expression : Ast.expression -> Catala_runtime.Value.t option
(** Handles literals, and enum constructors, structs, lists and tuples built
    from them. Returns [None] on anything that needs evaluating. *)
