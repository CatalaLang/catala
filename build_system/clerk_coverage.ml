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

open Catala_utils
open Shared_ast

module LineMap = Map.Make (struct
  include Int

  let format fmt x = Format.pp_print_int fmt x
end)

type aggregated_reached_code_coverage =
  (Pos_map.loc_interval * String.Set.t) list File.Map.t

type coverage_line_map = bool LineMap.t String.Map.t

let aggregated_code_coverage_to_coverage_line_map
    (full_coverage : Coverage.Aggregated_coverage.t) : coverage_line_map =
  Coverage.Aggregated_coverage.fold
    (fun pos coverage acc ->
      let file = Pos.get_file pos in
      let lines =
        let start = Pos.get_start_line pos in
        List.init (Pos.get_end_line pos - start + 1) (fun i -> start + i)
        |> List.map (fun l -> l, coverage > 0)
        |> LineMap.of_list
      in
      String.Map.update file
        (function
          | None -> Some lines
          | Some l -> Some (LineMap.union (fun _ l r -> Some (l && r)) l lines))
        acc)
    full_coverage String.Map.empty

let total_coverage_lines (line_map : coverage_line_map) =
  String.Map.fold (fun _ lines acc -> acc + LineMap.cardinal lines) line_map 0

let positive_coverage_lines (line_map : coverage_line_map) =
  String.Map.fold
    (fun _ lines acc ->
      LineMap.fold
        (fun _ cov_bool acc -> acc + if cov_bool then 1 else 0)
        lines acc)
    line_map 0

let negative_coverage_lines (line_map : coverage_line_map) =
  String.Map.fold
    (fun _ lines acc ->
      LineMap.fold
        (fun _ cov_bool acc -> acc + if cov_bool then 0 else 1)
        lines acc)
    line_map 0
