(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria, contributor:
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

type itv = { start_line : int; start_col : int; end_line : int; end_col : int }

let is_included
    itv
    { start_line = sl'; start_col = sc'; end_line = el'; end_col = ec' } =
  let is_in { start_line; start_col; end_line; end_col } (line, col) =
    (line > start_line && line < end_line)
    || line = start_line
       && line = end_col
       && start_col <= col
       && col <= end_col
    || (line > start_line && line = end_line && col <= end_col)
    || (line = start_line && line < end_line && start_col <= col)
  in
  is_in itv (sl', sc') && is_in itv (el', ec')

let format_itv ppf { start_line; start_col; end_line; end_col } =
  Format.fprintf ppf "%d.%d-%d.%d" start_line start_col end_line end_col

let from_pos p =
  {
    start_line = Pos.get_start_line p;
    start_col = Pos.get_start_column p;
    end_line = Pos.get_end_line p;
    end_col = Pos.get_end_column p;
  }

module ItvMap = Map.Make (struct
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

type 'a map = 'a ItvMap.t String.Map.t

let update p f m =
  let file = Pos.get_file p in
  let itv = from_pos p in
  String.Map.update file
    (function
      | None -> Option.map (fun v -> ItvMap.singleton itv v) (f None)
      | Some itvm -> Some (ItvMap.update itv f itvm))
    m

let union f m m' =
  String.Map.union
    (fun _ m m' -> Some (ItvMap.union (fun _itv v v' -> Some (f v v')) m m'))
    m m'

let map f m = String.Map.map (ItvMap.map f) m

type 'a interval_node = { itv : itv; v : 'a; children : 'a interval_tree }
and 'a interval_tree = 'a interval_node list

let map_to_interval_tree (m : 'a ItvMap.t) : 'a interval_tree =
  let bds = ItvMap.bindings m in
  let rec build_tree acc bds =
    match bds, acc with
    | [], acc -> List.rev acc
    | (itv, v) :: t, [] -> build_tree [{ itv; v; children = [] }] t
    | (itv, v) :: t, acc ->
      let inc, notinc =
        List.partition (fun { itv = itv'; _ } -> is_included itv itv') acc
      in
      build_tree ({ itv; v; children = List.rev inc } :: notinc) t
  in
  build_tree [] bds

let rec size : 'a interval_tree -> int = function
  | [] -> 0
  | { children; _ } :: t -> 1 + size children + size t

let rec format_interval_tree format_elt ppf itv_tree =
  let format_elt ppf { itv; v; children } =
    match children with
    | [] -> Format.fprintf ppf "@[%a -> %a@]" format_itv itv format_elt v
    | _ :: _ ->
      Format.fprintf ppf "@[<v 2>%a -> %a@ %a@]" format_itv itv format_elt v
        (format_interval_tree format_elt)
        children
  in
  Format.fprintf ppf "@[<v>%a@]"
    Format.(pp_print_list ~pp_sep:pp_print_cut format_elt)
    itv_tree
