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

module LineMap = Map.Make (struct
  include Int

  let format fmt x = Format.pp_print_int fmt x
end)

type aggregated_code_coverage =
  (Pos_map.loc_interval * Pos_map.coverage) list File.Map.t

type coverage_line_map = Pos_map.coverage LineMap.t File.Map.t

let aggregated_code_coverage_to_coverage_line_map
    (agg_cc : aggregated_code_coverage) : coverage_line_map =
  File.Map.fold
    (fun file locations acc ->
      let line_map : Pos_map.coverage LineMap.t =
        List.fold_left
          (fun (acc : Pos_map.coverage LineMap.t) (loc_interval, cov_kind) ->
            let open Pos_map in
            let lines_concerned =
              List.init
                (loc_interval.stop.line - loc_interval.start.line + 1)
                (fun i -> loc_interval.start.line + i)
            in
            List.fold_left
              (fun (acc : Pos_map.coverage LineMap.t) line_concerned ->
                let existing_cov_kind = LineMap.find_opt line_concerned acc in
                match existing_cov_kind, cov_kind with
                | None, cov_kind -> LineMap.add line_concerned cov_kind acc
                | Some Negative, _ -> acc
                | Some (Positive | Fulfilled), Negative ->
                  LineMap.add line_concerned cov_kind acc
                | _, _ -> acc)
              acc lines_concerned)
          LineMap.empty locations
      in
      File.Map.add file line_map acc)
    agg_cc File.Map.empty

let total_coverage_lines (line_map : coverage_line_map) =
  File.Map.fold (fun _ lines acc -> acc + LineMap.cardinal lines) line_map 0

let positive_coverage_lines (line_map : coverage_line_map) =
  File.Map.fold
    (fun _ lines acc ->
      LineMap.fold
        (fun _ cov_kind acc ->
          let open Pos_map in
          acc
          +
          match cov_kind with
          | Positive | Fulfilled -> 1
          | Negative | Reachable -> 0)
        lines acc)
    line_map 0

let negative_coverage_lines (line_map : coverage_line_map) =
  File.Map.fold
    (fun _ lines acc ->
      LineMap.fold
        (fun _ cov_kind acc ->
          let open Pos_map in
          acc
          +
          match cov_kind with
          | Positive | Fulfilled -> 0
          | Reachable | Negative -> 1)
        lines acc)
    line_map 0
