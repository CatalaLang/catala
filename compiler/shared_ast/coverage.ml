(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Florian Angeletti
   <florian.angeletti@inria.fr>

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

let glob = ref None
let is_recorded () = !glob <> None

let mark_position add p =
  Option.iter (fun map -> glob := Some (add p map)) !glob

let mark pol e = mark_position pol (Expr.mark_pos (Mark.get e))
let mark_pos p = mark Pos_map.pos p
let mark_neg p = mark Pos_map.neg p

let mark_all add e =
  Option.iter
    (fun m ->
      let m =
        List.fold_left (fun m x -> add (Expr.mark_pos @@ Mark.get x) m) m e
      in
      glob := Some m)
    !glob

type t = Reached | Reachable

let rec reachable e map =
  let m = Mark.get e in
  let loc = Expr.mark_pos m in
  let map = Pos_map.reachable loc map in
  Expr.shallow_fold reachable e map

module Coverage_map = Position_map.Make (struct
  type nonrec t = t

  let merge x y =
    match x, y with
    | Reachable, Reached | Reached, Reachable -> Reached
    | _ -> x

  let compare = compare

  let format fmt x =
    let rgb_of i = i / 256 / 256 mod 256, i / 256 mod 256, i mod 256 in
    let pale_green =
      let r24, g24, b24 = rgb_of 0xc7cb85 in
      Ocolor_types.C24 { r24; g24; b24 }
    in
    let orange =
      let r24, g24, b24 = rgb_of 0xe7a977 in
      Ocolor_types.C24 { r24; g24; b24 }
    in
    (match x with
    | Reached ->
      Format.pp_open_stag fmt Ocolor_format.(Ocolor_style_tag (Fg pale_green));
      Format.fprintf fmt "Reached"
    | Reachable ->
      Format.pp_open_stag fmt Ocolor_format.(Ocolor_style_tag (Fg orange));
      Format.fprintf fmt "Reachable");
    Format.pp_close_stag fmt ()
end)

module Aggregated_coverage = Position_map.Make (struct
  include Int

  let merge x y = x + y
  let format = Format.pp_print_int
end)

let to_aggregated_coverage m =
  Coverage_map.fold
    (fun pos coverage acc ->
      let n = if coverage = Reached then 1 else 0 in
      Aggregated_coverage.add pos n acc)
    m Aggregated_coverage.empty

let rec reachable_pos e map =
  match Mark.remove e with
  | Definitions.EAbs { binder; _ } ->
    (* skip lambdas *)
    let _vars, e' = Bindlib.unmbind binder in
    reachable_pos e' map
  | _ ->
    let m = Mark.get e in
    let loc = Expr.mark_pos m in
    let map = Coverage_map.add loc Reachable map in
    Expr.shallow_fold reachable_pos e map

let merge ~reachable_map reached_map =
  (* trim unreached files *)
  let reachable_map =
    Position_map.SMap.filter
      (fun f _ -> Position_map.SMap.mem f reached_map)
      reachable_map
  in
  let map = Coverage_map.merge reachable_map reached_map in
  let sanitized =
    Coverage_map.mapi_data
      (fun trie -> function
        | Reached -> Reached
        | Reachable ->
          if
            Coverage_map.Trie.all_children_data trie
            |> List.exists (( = ) Reached)
          then Reached
          else Reachable)
      map
  in
  sanitized

let union l r = Coverage_map.merge l r

let format_coverage_map ppf map =
  Hex.pp ppf (Hex.of_string (Marshal.to_string map []))

let from_new h =
  Hashtbl.fold (fun p x acc -> Pos_map.add p x acc) h Pos_map.empty
