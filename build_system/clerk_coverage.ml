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

open Catala_utils
open Shared_ast

type coverage_line_result = {
  total_reachable_lines : int;
  total_reached_lines : int;
  total_unreached_lines : int;
}

module LineMap = Map.Make (struct
  include Int

  let format fmt x = Format.pp_print_int fmt x
end)

let compute_coverage_per_line (coverage : Coverage.coverage_map) :
    coverage_line_result =
  let line_map =
    Coverage.fold
      (fun (file, { Coverage.start_line; end_line; _ }) coverage acc ->
        let lines =
          List.init (end_line - start_line + 1) (fun i -> start_line + i)
          |> List.map (fun l -> l, coverage <> Coverage.Unreached)
          |> LineMap.of_list
        in
        String.Map.update file
          (function
            | None -> Some lines
            | Some l ->
              Some
                (LineMap.union
                   (fun _ l r ->
                     (* Unreached lines stay unreached *)
                     Some (l && r))
                   l lines))
          acc)
      coverage String.Map.empty
  in
  let empty =
    {
      total_reachable_lines = 0;
      total_reached_lines = 0;
      total_unreached_lines = 0;
    }
  in
  String.Map.fold
    (fun _ lines acc ->
      LineMap.fold
        (fun _ cov_bool
             {
               total_reachable_lines;
               total_reached_lines;
               total_unreached_lines;
             } ->
          let total_reached_lines, total_unreached_lines =
            if cov_bool then succ total_reached_lines, total_unreached_lines
            else total_reached_lines, succ total_unreached_lines
          in
          {
            total_reachable_lines = succ total_reachable_lines;
            total_reached_lines;
            total_unreached_lines;
          })
        lines acc)
    line_map empty

let itv_to_vscode_range (li, i) (lj, j) =
  `Assoc
    [
      "start", `Assoc ["line", `Int (li - 1); "character", `Int (i - 1)];
      "end", `Assoc ["line", `Int (lj - 1); "character", `Int (j - 1)];
    ]

let itv_to_range { Coverage.start_line; start_col; end_line; end_col } =
  itv_to_vscode_range (start_line, start_col) (end_line, end_col)

let pos_to_json_range p =
  let open Pos in
  itv_to_vscode_range
    (get_start_line p, get_start_column p)
    (get_end_line p, get_end_column p)

let pfile_abs_path ~build_dir ~cwd file =
  File.(cwd / remove_prefix build_dir file)

let pos_to_json_location ~build_dir ~cwd pos =
  let file = Pos.get_file pos in
  `Assoc
    [
      "file", `String (pfile_abs_path ~build_dir ~cwd file);
      "range", pos_to_json_range pos;
    ]

let coverage_to_json ~build_dir ~cwd (coverage : Coverage.coverage_map) :
    Yojson.t =
  let itv_trees = Coverage.compute_interval_trees coverage in
  let all_scopes =
    Coverage.FileMap.fold
      (fun _ tree acc -> Coverage.ScopeSet.union (Coverage.all_scopes tree) acc)
      itv_trees Coverage.ScopeSet.empty
  in
  let scope_idx =
    let l =
      List.mapi (fun i s -> s, i) (Coverage.ScopeSet.elements all_scopes)
    in
    let scope_lookup s =
      List.find_opt (fun (s', _) -> Coverage.ScopeSet.same_scope s s') l
      |> Option.map snd
    in
    fun s ->
      match scope_lookup s with
      | None ->
        Message.error "Cannot find index of scope %a in coverage tree"
          ScopeName.format s
      | Some i -> `Int i
  in
  let itv_tree_to_json (f, tree) =
    let rec loop : Coverage.interval_tree -> [< `List of Yojson.t list ] =
      let open Coverage in
      function
      | [] -> `List []
      | { itv; cover; children } :: t ->
        let reached_by =
          match cover with
          | Unreached -> []
          | Reached_by { scopes } ->
            Coverage.ScopeSet.elements scopes |> List.map scope_idx
        in
        let node : Yojson.t =
          `Assoc
            [
              "range", itv_to_range itv;
              "reached_by", `List reached_by;
              "subtree", (loop children :> Yojson.t);
            ]
        in
        let (`List t) = loop t in
        `List (node :: t)
    in
    `Assoc
      [
        "file", `String (pfile_abs_path ~build_dir ~cwd f);
        "tree", (loop tree :> Yojson.t);
      ]
  in
  let scopes_index_json : string * Yojson.t =
    ( "scopes",
      `List
        (Coverage.ScopeSet.elements all_scopes
        |> List.map (fun s ->
               let pos = ScopeName.get_info s |> Mark.get in
               `Assoc
                 [
                   "index", scope_idx s;
                   "name", `String (ScopeName.to_string s);
                   "location", pos_to_json_location ~build_dir ~cwd pos;
                 ])) )
  in
  let coverage_locations_json =
    ( "locations",
      `List (Coverage.FileMap.bindings itv_trees |> List.map itv_tree_to_json) )
  in
  `Assoc [scopes_index_json; coverage_locations_json]
