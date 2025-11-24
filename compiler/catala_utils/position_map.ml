(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
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

module type Data = sig
  type t

  val compare : t -> t -> int
  val format : Format.formatter -> t -> unit
end

module Make_trie (D : Data) = struct
  module DS = Set.Make (D)

  type itv = (int (* line *) * int (* col *)) * (int (* line *) * int (* col *))

  type node = Node of { itv : itv; data : DS.t; children : trie }
  and trie = node list

  type t = trie

  let pp_itv ppf ((li, i), (lj, j)) =
    Format.fprintf ppf "(%d:%d)⟷(%d:%d)" li i lj j

  let rec pp_node ppf (Node { itv; data; children }) =
    let open Format in
    match children with
    | [] ->
      fprintf ppf "@[<h>%a: [ @[<h>%a@] ]@]" pp_itv itv
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ; ") D.format)
        (DS.elements data)
    | _ ->
      fprintf ppf "@[<v 2>%a: [ @[<h>%a@] ]@ %a@]" pp_itv itv
        (pp_print_list ~pp_sep:pp_print_space D.format)
        (DS.elements data) pp_trie children

  and pp_trie ppf tries =
    let open Format in
    fprintf ppf "@[<v>%a@]" (pp_print_list ~pp_sep:pp_print_cut pp_node) tries

  let is_in ((li, i), (lj, j)) (ln, n) =
    (ln > li && ln < lj)
    || (ln = li && ln = lj && i <= n && n <= j)
    || (ln > li && ln = lj && n <= j)
    || (ln = li && ln < lj && i <= n)

  let is_in_line ((li, _), (lj, _)) ln = ln >= li && ln <= lj
  let is_included itv (left, right) = is_in itv left && is_in itv right

  let rec lookup i trie =
    List.find_opt (fun (Node { itv; _ }) -> is_in itv i) trie
    |> function
    | None -> None
    | Some (Node { children = []; data; _ }) -> Some data
    | Some (Node { children; data; _ }) -> (
      match lookup i children with None -> Some data | Some v -> Some v)

  let all_nodes trie =
    let rev_nodes = ref [] in
    let rec loop (Node { children; _ } as x) =
      rev_nodes := x :: !rev_nodes;
      List.iter loop children
    in
    List.iter loop trie;
    List.rev !rev_nodes

  let lookup_on_line l trie =
    List.find_all (fun (Node { itv; _ }) -> is_in_line itv l) trie
    |> all_nodes
    |> List.filter (fun (Node { itv = (li, _), (lj, _); _ }) ->
           not (li <> l || lj <> l))
    |> List.map (fun (Node { data; _ }) -> data)
    |> function [] -> None | l -> Some l

  let lookup_best_on_line (l : int) trie : DS.t option =
    match List.find_all (fun (Node { itv; _ }) -> is_in_line itv l) trie with
    | [] -> None
    | candidates -> (
      let compare_candidate
          (Node { itv = (li, i), _; _ })
          (Node { itv = (li', i'), _; _ }) =
        (* hypothesis: contains [l] *)
        let dist x =
          let abs x = if x < 0 then -x else x in
          abs (x - l)
        in
        if li = li' then compare i i' else compare (dist li) (dist li')
      in
      let sorted_candidates =
        List.sort compare_candidate (all_nodes candidates)
      in
      match sorted_candidates with
      | [] -> assert false
      | Node { data; _ } :: _ -> Some data)

  let compare_itv (((li, i), (lj, j)) as itv) (((li', i'), (lj', j')) as itv') =
    if itv = itv' then `Equal
    else if is_included itv itv' then `Subset
    else if is_included itv' itv then `Supset
    else if lj' < li || (lj' = li && j' < i) then `Disjoint_left
    else if li' > lj || (li' = lj && i' > j) then `Disjoint_right
    else if is_in itv (li', i') then
      `Left_inclusion
        ( ((li', i'), (lj, j)) (* included part *),
          ((lj, j + 1), (lj', j')) (* disjoint part *) )
    else (
      assert (is_in itv (lj', j'));
      `Right_inclusion
        ( ((li', i'), (li, i - 1)) (* disjoint part *),
          ((li, i), (lj', j')) (* included part *) ))

  let rec gather_children (Node { children; _ }) =
    let fresh_children =
      List.map (fun (Node node) -> Node { node with children = [] }) children
    in
    fresh_children @ List.concat_map gather_children children

  let rec insert_all itv (data : DS.t) trie =
    let rec find_included_nodes acc = function
      | [] -> List.rev acc, `Disjoint []
      | (Node n as h) :: t as l -> (
        match compare_itv itv n.itv with
        | `Equal -> assert false
        | `Subset -> find_included_nodes (h :: acc) t
        | `Disjoint_right -> List.rev acc, `Disjoint l
        | `Disjoint_left | `Supset | `Right_inclusion _ ->
          (* by construction *) assert false
        | `Left_inclusion (included_itv, _disjoint_part) ->
          acc, `Joint (included_itv, l))
    in
    let rec loop acc itv : trie -> trie = function
      | [] -> Node { itv; data; children = [] } :: acc |> List.rev
      | (Node ({ itv = itv_p; children; _ } as node_r) as node) :: t as l -> (
        match compare_itv itv_p itv with
        | `Equal ->
          let node = Node { node_r with data = DS.union data node_r.data } in
          List.rev_append (node :: acc) t
        | `Disjoint_left ->
          List.rev_append (Node { itv; data; children = [] } :: acc) l
        | `Disjoint_right -> loop (node :: acc) itv t
        | `Subset ->
          let new_children = insert_all itv data children in
          List.rev_append
            (Node { node_r with children = new_children } :: acc)
            t
        | `Supset -> (
          let all_included_nodes, r = find_included_nodes [] l in
          match r with
          | `Disjoint l ->
            let node = Node { itv; data; children = all_included_nodes } in
            List.rev_append (node :: acc) l
          | `Joint (included_itv, r) ->
            let (_, i'), _ = included_itv in
            let (li, i), (lj, _) = itv in
            let node =
              Node
                {
                  itv = (li, i), (lj, i' - 1);
                  data;
                  children = all_included_nodes;
                }
            in
            List.rev_append (node :: acc) (loop [] included_itv r))
        | `Left_inclusion (included_itv, disjoint_part) ->
          let new_children = insert_all included_itv data children in
          loop
            (Node { node_r with children = new_children } :: acc)
            disjoint_part t
        | `Right_inclusion (disjoint_part, included_itv) ->
          let acc = Node { itv = disjoint_part; data; children = [] } :: acc in
          let new_children = insert_all included_itv data children in
          List.rev_append
            (Node { node_r with children = new_children } :: acc)
            t)
    in
    loop [] itv trie

  let rec map_data f trie =
    List.map
      (fun (Node { itv; data; children }) ->
        Node { itv; data = f data; children = map_data f children })
      trie
end

module SMap = Map.Make (String)

module Make (D : Data) = struct
  module Trie = Make_trie (D)
  module DS = Trie.DS

  type pmap = Trie.t SMap.t
  type t = pmap

  let pp ppf pmap =
    let open Format in
    fprintf ppf "@[<v 2>variables:@ %a@]"
      (SMap.format ~pp_sep:pp_print_cut Trie.pp_trie)
      pmap

  let ( -- ) i j = List.init (j - i + 1) (fun x -> i + x)

  let merge_tries _i trie trie' : Trie.t option =
    match trie, trie' with
    | None, None -> None
    | None, Some (itv, data) -> Some [Node { itv; data; children = [] }]
    | Some trie, None -> Some trie
    | Some trie, Some (itv, data) -> Some (Trie.insert_all itv data trie)

  let pos_to_itv pos =
    let li = Pos.get_start_line pos in
    let i = Pos.get_start_column pos in
    let lj = Pos.get_end_line pos in
    let j = Pos.get_end_column pos in
    (li, i), (lj, j)

  let pp_raw ppf (p, d) =
    let open Format in
    fprintf ppf "@[<h>%a: %a@]" Trie.pp_itv (pos_to_itv p) D.format d

  let add_all pos data variables =
    if pos = Pos.void then variables
    else
      let itv = pos_to_itv pos in
      let data = DS.of_list data in
      SMap.update (Pos.get_file pos)
        (function
          | None -> Some [Trie.Node { itv; data; children = [] }]
          | Some trie -> Some (Trie.insert_all itv data trie))
        variables

  let add pos data variables = add_all pos [data] variables

  let lookup pos pmap =
    let ( let* ) = Option.bind in
    (* we assume that pos's start/end lines, start/end column are equal *)
    let* trie = SMap.find_opt (Pos.get_file pos) pmap in
    Trie.lookup (Pos.get_start_line pos, Pos.get_start_column pos) trie

  let fold f pmap acc =
    let rec fold file trie acc =
      List.fold_left
        (fun acc (Trie.Node { itv = (li, i), (lj, j); children; data }) ->
          let acc = f (Pos.from_info file li i lj j) data acc in
          fold file children acc)
        acc trie
    in
    SMap.fold fold pmap acc

  let fold_on_file file f pmap acc =
    SMap.find_opt file pmap
    |> function
    | None -> acc | Some pmap -> fold f (SMap.singleton file pmap) acc

  let map_data f pmap = SMap.map (fun trie -> Trie.map_data f trie) pmap
  let iter f pmap = fold (fun k v () -> f k v) pmap ()
  let empty = SMap.empty
end
