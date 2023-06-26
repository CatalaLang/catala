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

(** Three-way minimum *)
let minimum a b c = min a (min b c)

(** Computes the levenshtein distance between two strings, used to provide error
    messages suggestions *)
let levenshtein_distance (s : string) (t : string) : int =
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
          minimum
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
  let rec insertion ((new_x, new_y) : int * 'a) (n_tuple_list : (int * 'a) list)
      : (int * 'a) list =
    match n_tuple_list with
    | (current_x, current_y) :: tail ->
      if new_x <= current_x then (new_x, new_y) :: n_tuple_list
        (*= to satisfy first-come first-served basis (because the last element
          is inserted first (see levenshtein_distance_association))*)
      else (current_x, current_y) :: insertion (new_x, new_y) tail
    | [] -> [new_x, new_y]
  in
  (*Here we associate each elements of "string_list'" with its levenshtein
    distance with "keyword'"*)
  (*It returns a 2-tuple list with the following format (levenshein_distance,
    word_from_string_list). 2-tuples are sorted on the first-come first-served
    basis*)
  let rec levenshtein_distance_association
      (string_list' : string list)
      (keyword' : string) : (int * string) list =
    match string_list' with
    | h :: t ->
      insertion
        (levenshtein_distance h keyword', h)
        (levenshtein_distance_association t keyword')
    | [] -> []
  in
  let final_list = levenshtein_distance_association string_list keyword in
  match final_list with
  | h :: _ ->
    (*We collect the strings from "string_list" with the minimum levenshtein
      distance found (i.e. the distance of the first element of the sorted
      list*)
    List.map snd (List.filter (fun (x, _) -> x == fst h) final_list)
    (*< impossible because the list is already sorted in ascending order*)
  | [] -> []
