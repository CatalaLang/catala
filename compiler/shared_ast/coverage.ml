(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Florian Angeletti
   <florian.angeletti@inria.fr>, Vincent Botbol <vincent.botbol@inria.fr>

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
open Definitions

module FileMap = Map.Make (struct
  (* We want to avoid costly & unnecessary custom comparison functions *)
  include Stdlib.String

  let format = Format.pp_print_string
end)

module ScopeSet = struct
  let scope_compare s s' =
    Pos.compare (ScopeName.get_info s |> snd) (ScopeName.get_info s' |> snd)

  let same_scope s s' = scope_compare s s' = 0

  include Set.Make (struct
    type t = ScopeName.t

    let compare = scope_compare
  end)
end

type cover = Unreached | Reached_by of { scopes : ScopeSet.t }
type itv = { start_line : int; start_col : int; end_line : int; end_col : int }

let format_cover ppf =
  let open Format in
  function
  | Unreached -> fprintf ppf "Unreached"
  | Reached_by { scopes } ->
    fprintf ppf "Reached by [ %a ]"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") ScopeName.format)
      (ScopeSet.elements scopes)

let format_itv ppf { start_line; start_col; end_line; end_col } =
  Format.fprintf ppf "%d.%d-%d.%d" start_line start_col end_line end_col

let is_included
    itv
    { start_line = sl'; start_col = sc'; end_line = el'; end_col = ec' } =
  let is_in { start_line; start_col; end_line; end_col } (line, col) =
    (line > start_line && line < end_line)
    || line = start_line
       && line = end_line
       && start_col <= col
       && col <= end_col
    || (line > start_line && line = end_line && col <= end_col)
    || (line = start_line && line < end_line && start_col <= col)
  in
  is_in itv (sl', sc') && is_in itv (el', ec')

let from_pos p =
  {
    start_line = Pos.get_start_line p;
    start_col = Pos.get_start_column p;
    end_line = Pos.get_end_line p;
    end_col = Pos.get_end_column p;
  }

module ItvMap = Catala_utils.Map.Make (struct
  type t = itv

  let format = format_itv

  (* This order yields an interval tree's prefix-order: for all pairs of
     consecutive elements (A, B), we have A's interval included in B's or A's
     interval is disjoint and located before B's interval.

     This allows to easily build an interval tree.

     Hypothesis: no interval elements are overlapping *)
  let compare t1 t2 =
    let n = Int.compare t1.end_line t2.end_line in
    if n <> 0 then n
    else
      let n = Int.compare t1.end_col t2.end_col in
      if n <> 0 then n
      else
        let n = Int.compare t2.start_line t1.start_line in
        if n <> 0 then n
        else
          let n = Int.compare t2.start_col t1.start_col in
          if n <> 0 then n else n
end)

type coverage_map = cover ItvMap.t FileMap.t

let empty = FileMap.empty

let update p f m =
  let file = Pos.get_file p in
  if String.equal file "" then m
  else
    let itv = from_pos p in
    FileMap.update file
      (function
        | None -> Option.map (fun v -> ItvMap.singleton itv v) (f None)
        | Some itvm -> Some (ItvMap.update itv f itvm))
      m

let reached_pos p s (m : coverage_map) : coverage_map =
  update p
    (function
      | None | Some Unreached ->
        Some (Reached_by { scopes = ScopeSet.singleton s })
      | Some (Reached_by _ as x) -> Some x)
    m

let unreached_pos p (m : coverage_map) : coverage_map =
  update p
    (function
      | None | Some Unreached -> Some Unreached
      | Some (Reached_by _ as elt) -> Some elt)
    m

let reachable_positions p =
  Program.fold_exprs
    ~f:(fun acc e _typ ->
      let rec loop e map =
        match Mark.remove e with
        | Definitions.EAbs { binder; _ } ->
          (* skip lambdas *)
          let _vars, e' = Bindlib.unmbind binder in
          loop e' map
        | _ ->
          let pos = Expr.pos e in
          let map = unreached_pos pos map in
          Expr.shallow_fold loop e map
      in
      loop e acc)
    p ~init:empty

let merge_cover v v' =
  match v, v' with
  | Reached_by s, Unreached | Unreached, Reached_by s -> Reached_by s
  | Unreached, Unreached -> Unreached
  | Reached_by { scopes = s }, Reached_by { scopes = s' } ->
    Reached_by { scopes = ScopeSet.union s s' }

let union m m' =
  FileMap.union
    (fun _ m m' ->
      Some (ItvMap.union (fun _itv v v' -> Some (merge_cover v v')) m m'))
    m m'

let merge_with_reachable_positions ~reachable ~reached =
  (* Remove unreached files *)
  FileMap.merge
    (fun _k reachable reached -> if reached = None then None else reachable)
    reachable reached
  |> union reached

let filter_files f covmap = FileMap.filter (fun file _ -> f file) covmap

let fold f m acc =
  FileMap.fold
    (fun file itvm acc ->
      ItvMap.fold (fun itv v acc -> f (file, itv) v acc) itvm acc)
    m acc

type interval_tree = interval_node list
and interval_node = { itv : itv; cover : cover; children : interval_tree }

let sanitize_interval_tree (tree : interval_tree) : interval_tree =
  (* In some cases, we have a cover that is not a super-set of its children:
     e.g., Unreached parent with Reached_by children. We normalize them. *)
  let rec normalize_unreachable : interval_tree -> interval_tree = function
    | [] -> []
    | { itv; cover; children } :: t ->
      let children = normalize_unreachable children in
      let cover =
        List.fold_left
          (fun acc { cover; _ } -> merge_cover cover acc)
          cover children
      in
      { itv; cover; children } :: normalize_unreachable t
  in
  let rec merge_siblings : interval_tree -> interval_tree = function
    | [] -> []
    | ({ cover = Unreached; _ } as h) :: t ->
      (* Unreached nodes (must) only have unreached children, we merge them
         together *)
      { h with children = [] } :: merge_siblings t
    | ({ itv = _; cover = Reached_by { scopes = s }; children } as h) :: t ->
      let children = merge_siblings children in
      let new_children =
        (* We merge children with an equivalent cover by lifting them to the
           parent and along with their children. *)
        List.fold_left
          (fun acc -> function
            | { itv = _; cover = Reached_by { scopes = s' }; children } as x ->
              if ScopeSet.equal s s' then List.rev_append children acc
              else x :: acc
            | x -> x :: acc)
          [] children
        |> List.rev
      in
      { h with children = new_children } :: merge_siblings t
  in
  normalize_unreachable tree |> merge_siblings

let map_to_interval_tree (m : 'a ItvMap.t) : interval_tree =
  let bds = ItvMap.bindings m in
  let rec included_partition acc itv l =
    match l with
    | [] -> acc, []
    | ({ itv = itv'; _ } as elt) :: t ->
      if is_included itv itv' then included_partition (elt :: acc) itv t
      else acc, elt :: t
  in
  let rec build_tree acc bds =
    match bds, acc with
    | [], acc -> List.rev acc
    | (itv, cover) :: t, acc ->
      let inc, notinc = included_partition [] itv acc in
      build_tree ({ itv; cover; children = inc } :: notinc) t
  in
  build_tree [] bds |> sanitize_interval_tree

let compute_interval_trees cov_map = FileMap.map map_to_interval_tree cov_map

let rec fold_interval_tree f t init =
  List.fold_left
    (fun acc { itv; cover; children } ->
      let acc = fold_interval_tree f children acc in
      f (itv, cover) acc)
    init t

let all_scopes t =
  fold_interval_tree
    (fun (_itv, cover) acc ->
      match cover with
      | Reached_by { scopes } -> ScopeSet.union acc scopes
      | Unreached -> acc)
    t ScopeSet.empty

let rec format_interval_tree ppf itv_tree =
  let format_node ppf { itv; cover; children } =
    match children with
    | [] -> Format.fprintf ppf "@[%a -> %a@]" format_itv itv format_cover cover
    | _ :: _ ->
      Format.fprintf ppf "@[<v 2>%a -> %a@ %a@]" format_itv itv format_cover
        cover format_interval_tree children
  in
  Format.fprintf ppf "@[<v>%a@]"
    Format.(pp_print_list ~pp_sep:pp_print_cut format_node)
    itv_tree

let format_coverage_hex_dump ppf (map : coverage_map) =
  Hex.pp ppf (Hex.of_string (Marshal.to_string map []))

let of_hex hex_dump =
  let hex_coverage = `Hex hex_dump in
  let hex_coverage_bytes = Hex.to_bytes hex_coverage in
  try Marshal.from_bytes hex_coverage_bytes 0
  with _ -> Message.error "Failed to decode coverage"
