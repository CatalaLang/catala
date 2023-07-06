(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria, contributor:
   Aminata Boiguillé <aminata.boiguille@etu.sorbonne-universite.fr>, Emile
   Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Computes the levenshtein distance between two strings, used to provide error
    messages suggestions *)
let levenshtein_distance (s : string) (t : string) : int =
  (* Three-way minimum *)
  let three_way_minimum a b c = min a (min b c) in
  let m = String.length s and n = String.length t in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between the
     first i characters of s and the first j characters of t *)
  let d = Array.make_matrix (m + 1) (n + 1) 0 in

  for i = 0 to m do
    d.(i).(0) <- i
    (* the distance of any first string to an empty second string *)
  done;
  for j = 0 to n do
    d.(0).(j) <- j
    (* the distance of any second string to an empty first string *)
  done;

  for j = 1 to n do
    for i = 1 to m do
      if s.[i - 1] = t.[j - 1] then d.(i).(j) <- d.(i - 1).(j - 1)
        (* no operation required *)
      else
        d.(i).(j) <-
          three_way_minimum
            (d.(i - 1).(j) + 1) (* a deletion *)
            (d.(i).(j - 1) + 1) (* an insertion *)
            (d.(i - 1).(j - 1) + 1) (* a substitution *)
    done
  done;

  d.(m).(n)

(*We're creating a string list composed by those who satisfy the following rule
  : they share the same levenshtein distance, which is the minimal distance
  found between the reference word "keyword" and all the strings in
  "string_list".*)
let suggestion_minimum_levenshtein_distance_association
    (string_list : string list)
    (keyword : string) : string list =
  let rec strings_minimum_levenshtein_distance
      (minimum : int)
      (result : string list)
      (levenshtein_distance_association' : (string * int) list) : string list =
    match levenshtein_distance_association' with
    | (current_string, current_lev_dist) :: tail ->
      if current_lev_dist < minimum then
        strings_minimum_levenshtein_distance current_lev_dist [current_string]
          tail
      else if current_lev_dist = minimum then
        strings_minimum_levenshtein_distance minimum
          (result @ [current_string])
          tail
      else strings_minimum_levenshtein_distance minimum result tail
    | _ -> result
  in
  let levenshtein_distance_association =
    List.map (fun s -> s, levenshtein_distance keyword s) string_list
  in
  match levenshtein_distance_association with
  | [] -> []
  | (_, first_levenshtein_distance) :: _ ->
    strings_minimum_levenshtein_distance first_levenshtein_distance []
      levenshtein_distance_association
