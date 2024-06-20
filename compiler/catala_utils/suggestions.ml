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

module M = Stdlib.Map.Make (Int)

let compute_candidates (candidates : string list) (word : string) :
    string list M.t =
  List.fold_left
    (fun m candidate ->
      let distance = levenshtein_distance word candidate in
      M.update distance
        (function None -> Some [candidate] | Some l -> Some (candidate :: l))
        m)
    M.empty candidates

let best_candidates candidates word =
  let candidates = compute_candidates candidates word in
  M.choose_opt candidates |> function None -> [] | Some (_, l) -> List.rev l

let sorted_candidates ?(max_elements = 5) suggs given =
  let rec sub acc n = function
    | [] -> List.rev acc
    | x :: t when n > 0 -> sub (x :: acc) (pred n) t
    | _ -> List.rev acc
  in
  let candidates =
    List.map
      (fun (_, l) -> List.rev l)
      (M.bindings (compute_candidates suggs given))
  in
  List.concat candidates |> sub [] max_elements

let format (ppf : Format.formatter) (suggestions_list : string list) =
  match suggestions_list with
  | [] -> ()
  | _ :: _ ->
    Format.pp_print_string ppf "Maybe you wanted to write : ";
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ or ")
      (fun ppf string -> Format.fprintf ppf "@{<yellow>\"%s\"@}" string)
      ppf suggestions_list;
    Format.pp_print_string ppf " ?"
