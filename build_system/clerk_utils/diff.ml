(*
   Copyright (c) 2017 Gabriel Jaldon gjaldon85@gmail.com

   Permission to use, copy, modify, and distribute this software for any purpose
   with or without fee is hereby granted, provided that the above copyright
   notice and this permission notice appear in all copies.

   https://github.com/gjaldon/simple-diff/

   Edited by Martin Jambon <martin@semgrep.com>

   https://github.com/mjambon/testo/tree/main/diff

   "deriving" annotations removed for Catala
*)

module type Comparable = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type item

  type diff =
    | Deleted of item array
    | Added of item array
    | Equal of item array

  type t = diff list

  val get_diff : item array -> item array -> t

  val recover_input : t -> item array * item array
  (** Recover the original input passed to {!get_diff}. *)
end

module Make (Item : Comparable) = struct
  type item = Item.t

  type diff =
    | Deleted of item array
    | Added of item array
    | Equal of item array

  type t = diff list

  type subsequence_info = {
    (* Starting index of longest subsequence in the list of new values *)
    sub_start_new : int;
    (* Starting index of longest subsequence in the list of old values *)
    sub_start_old : int;
    (* The length of the longest subsequence *)
    longest_subsequence : int;
  }

  module CounterMap = Map.Make (Item)

  (* Returns a map with the line as key and a list of indices as value.
     Represents counts of all the lines. *)
  let map_counter keys =
    let keys_and_indices = Array.mapi (fun index key -> index, key) keys in
    Array.fold_left
      (fun map (index, key) ->
        let indices = try CounterMap.find key map with Not_found -> [] in
        CounterMap.add key (index :: indices) map)
      CounterMap.empty keys_and_indices

  (* Computes longest subsequence and returns data on the length of longest
     subsequence and the starting index for the longest subsequence in the old
     and new versions. *)
  let get_longest_subsequence old_lines new_lines =
    let prev_overlap = ref (Hashtbl.create 0) in
    let old_values_counter = map_counter old_lines in
    let sub_start_old = ref 0 in
    let sub_start_new = ref 0 in
    let longest_subsequence = ref 0 in

    Array.iteri
      (fun new_index new_value ->
        let indices =
          try CounterMap.find new_value old_values_counter
          with Not_found -> []
        in
        let overlap = Hashtbl.create 100 in
        List.iter
          (fun old_index ->
            let prev_subsequence =
              try Hashtbl.find !prev_overlap (old_index - 1)
              with Not_found -> 0
            in
            let new_subsequence = prev_subsequence + 1 in
            Hashtbl.add overlap old_index new_subsequence;

            if new_subsequence > !longest_subsequence then (
              sub_start_old := old_index - new_subsequence + 1;
              sub_start_new := new_index - new_subsequence + 1;
              longest_subsequence := new_subsequence))
          indices;
        prev_overlap := overlap)
      new_lines;

    {
      sub_start_new = !sub_start_new;
      sub_start_old = !sub_start_old;
      longest_subsequence = !longest_subsequence;
    }

  let rec get_diff old_lines new_lines =
    match old_lines, new_lines with
    | [||], [||] -> []
    | _, _ ->
      let { sub_start_new; sub_start_old; longest_subsequence } =
        get_longest_subsequence old_lines new_lines
      in

      if longest_subsequence == 0 then [Deleted old_lines; Added new_lines]
      else
        let old_lines_presubseq = Array.sub old_lines 0 sub_start_old in
        let new_lines_presubseq = Array.sub new_lines 0 sub_start_new in
        let old_lines_postsubseq =
          let start_index = sub_start_old + longest_subsequence in
          let end_index = Array.length old_lines - start_index in
          Array.sub old_lines start_index end_index
        in
        let new_lines_postsubseq =
          let start_index = sub_start_new + longest_subsequence in
          let end_index = Array.length new_lines - start_index in
          Array.sub new_lines start_index end_index
        in
        let unchanged_lines =
          Array.sub new_lines sub_start_new longest_subsequence
        in
        get_diff old_lines_presubseq new_lines_presubseq
        @ [Equal unchanged_lines]
        @ get_diff old_lines_postsubseq new_lines_postsubseq

  let recover_old_input diffs =
    List.fold_left
      (fun acc diff ->
        match diff with Deleted x | Equal x -> x :: acc | Added _ -> acc)
      [] diffs
    |> List.rev
    |> Array.concat

  let recover_new_input diffs =
    List.fold_left
      (fun acc diff ->
        match diff with Added x | Equal x -> x :: acc | Deleted _ -> acc)
      [] diffs
    |> List.rev
    |> Array.concat

  (* This is intended for tests, that's why we return arrays like in the
     original input even though lists are usually more convenient for
     the user. *)
  let recover_input diffs = recover_old_input diffs, recover_new_input diffs
end
