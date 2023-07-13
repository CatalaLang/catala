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

(*We create a list composed by strings that satisfy the following rule : they
  have the same levenshtein distance, which is the minimum distance between the
  reference word "keyword" and all the strings in "candidates" (with the
  condition that this minimum is equal to or less than one third of the length
  of keyword + 1, in order to get suggestions close to "keyword")*)
let suggestion_minimum_levenshtein_distance_association
    (candidates : string list)
    (keyword : string) : string list =
  let rec strings_minimum_levenshtein_distance
      (minimum : int)
      (result : string list)
      (candidates' : string list) : string list =
    (*As we iterate through the "candidates'" list, we create a list "result"
      with all strings that have the last minimum levenshtein distance found
      ("minimum").*)
    match candidates' with
    (*When a new minimum levenshtein distance is found, the new result list is
      our new element "current_string" followed by strings that have the same
      minimum distance. It will be the "result" list if there is no levenshtein
      distance smaller than this new minimum.*)
    | current_string :: tail ->
      let current_levenshtein_distance =
        levenshtein_distance current_string keyword
      in
      if current_levenshtein_distance < minimum then
        strings_minimum_levenshtein_distance current_levenshtein_distance
          [current_string] tail
        (*The "result" list is updated (we append "current_string" to "result")
          when a new string shares the same minimum levenshtein distance
          "minimum"*)
      else if current_levenshtein_distance = minimum then
        strings_minimum_levenshtein_distance minimum
          (result @ [current_string])
          tail
        (*If a levenshtein distance greater than the minimum is found, "result"
          doesn't change*)
      else strings_minimum_levenshtein_distance minimum result tail
    (*The "result" list is returned at the end of the "candidates'" list.*)
    | [] -> result
  in
  strings_minimum_levenshtein_distance
    (1 + (String.length keyword / 3))
    (*In order to select suggestions that are not too far away from the
      keyword*)
    [] candidates

let format (ppf : Format.formatter) (suggestions_list : string list) =
  match suggestions_list with
  | [] -> ()
  | _ :: _ ->
    Format.pp_print_string ppf "Maybe you wanted to write : ";
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ or ")
      (fun ppf string -> Format.fprintf ppf "@{<yellow>\"%s\"@}" string)
      ppf suggestions_list
