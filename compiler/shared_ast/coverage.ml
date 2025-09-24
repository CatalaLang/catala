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
          List.fold_left
            (fun m x -> add (Expr.mark_pos @@ Mark.get x) m)
            m e
        in
        glob := Some m)
      !glob

    let rec reachable e map =
      let m = Mark.get e in
      let loc = Expr.mark_pos m in
      let map = Pos_map.reachable loc map in
      Expr.shallow_fold reachable e map
