(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

module type Comparable = sig
  type t

  val compare : t -> t -> int
end

module Make (X : Comparable) = struct
  type item = X.t

  type diff = Deleted of item list | Added of item list | Equal of item list

  type t = diff list

  module ResultTable = Map.Make (struct
    type t = int * int

    let compare (x1, x2) (y1, y2) = if x1 = y1 then x2 - y2 else x1 - y1
  end)

  (* TODO: optimize this ! *)
  let rec longest_common_subsequence (results : item list ResultTable.t) (x1 : item array)
      (x2 : item array) (i1 : int) (i2 : int) : item list * item list ResultTable.t =
    if ResultTable.mem (i1, i2) results then (ResultTable.find (i1, i2) results, results)
    else if i1 = 0 || i2 = 0 then ([], ResultTable.add (0, 0) [] results)
    else if X.compare x1.(i1 - 1) x2.(i2 - 1) = 0 then
      let res, new_results = longest_common_subsequence results x1 x2 (i1 - 1) (i2 - 1) in
      let res = res @ [ x1.(i1 - 1) ] in
      (res, ResultTable.add (i1, i2) res new_results)
    else
      let res1, new_results1 = longest_common_subsequence results x1 x2 (i1 - 1) i2 in
      let res2, new_results2 = longest_common_subsequence new_results1 x1 x2 i1 (i2 - 1) in
      let res = if List.length res1 > List.length res2 then res1 else res2 in
      (res, ResultTable.add (i1, i2) res new_results2)

  let rec get_diff_aux (x1 : item array) (x2 : item array) (i1 : int) (i2 : int) (lcs : item list) :
      diff list =
    if i1 >= Array.length x1 && i2 >= Array.length x2 then [ Equal [] ]
    else if i1 >= Array.length x1 then
      [ Added (Array.to_list (Array.sub x2 i2 (Array.length x2 - i2))) ]
    else if i2 >= Array.length x2 then
      [ Deleted (Array.to_list (Array.sub x1 i1 (Array.length x1 - i1))) ]
    else
      match lcs with
      | [] ->
          [
            Deleted (Array.to_list (Array.sub x1 i1 (Array.length x1 - i1)));
            Added (Array.to_list (Array.sub x2 i2 (Array.length x2 - i2)));
          ]
      | hd :: lcs_rest ->
          if X.compare x1.(i1) hd = 0 && X.compare x2.(i2) hd = 0 then
            Equal [ hd ] :: get_diff_aux x1 x2 (i1 + 1) (i2 + 1) lcs_rest
          else if X.compare x1.(i1) hd = 0 then
            Added [ x2.(i2) ] :: get_diff_aux x1 x2 i1 (i2 + 1) lcs
          else if X.compare x2.(i2) hd = 0 then
            Deleted [ x1.(i1) ] :: get_diff_aux x1 x2 (i1 + 1) i2 lcs
          else
            let after = get_diff_aux x1 x2 (i1 + 1) (i2 + 1) lcs in
            Deleted [ x1.(i1) ] :: Added [ x2.(i2) ] :: after

  let compress_t (x : t) : t =
    List.rev
      (List.fold_left
         (fun (acc : t) (diff : diff) ->
           match (acc, diff) with
           | [], _ -> [ diff ]
           | Added x1 :: rest_acc, Added x2 -> Added (x1 @ x2) :: rest_acc
           | Deleted x1 :: rest_acc, Deleted x2 -> Deleted (x1 @ x2) :: rest_acc
           | Equal x1 :: rest_acc, Equal x2 -> Equal (x1 @ x2) :: rest_acc
           | _ -> diff :: acc)
         [] x)

  let get_diff (x1 : item array) (x2 : item array) : t =
    let lcs, _ =
      longest_common_subsequence ResultTable.empty x1 x2 (Array.length x1) (Array.length x2)
    in
    let out = get_diff_aux x1 x2 0 0 lcs in
    compress_t out
end
