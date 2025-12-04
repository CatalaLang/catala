(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2025 Inria,
   contributor: Denis Merigoux <denis.merigoux@inria.fr>, Vincent Botbol
   <vincent.botbol@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Shared_ast
open Catala_utils

type coverage_line_result = {
  total_reachable_lines : int;
  total_reached_lines : int;
  total_unreached_lines : int;
}

val pos_to_json_range : Pos.t -> Yojson.t
val pos_to_json_location : build_dir:string -> cwd:string -> Pos.t -> Yojson.t
val compute_coverage_per_line : Coverage.coverage_map -> coverage_line_result

val coverage_to_json :
  build_dir:string -> cwd:string -> Coverage.coverage_map -> Yojson.t
