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

val levenshtein_distance : string -> string -> int
(** [levenshtein_distance w1 w2] computes the levenshtein distance separating
    [w1] from [w2]. *)

val best_candidates : string list -> string -> string list
(** [best_candidates suggestions word] returns the subset of elements in
    [suggestions] that minimize the levenshtein distance to [word]. Multiple
    candidates that have a same distance is possible. *)

val sorted_candidates :
  ?max_elements:int -> string list -> string -> string list
(** [sorted_candidates ?max_elements suggestions word] sorts the [suggestions]
    list and retain at most [max_elements] (defaults to 5). This list is ordered
    by their levenshtein distance to [word], i.e., the first elements are the
    most similar. *)

val format : Format.formatter -> string list -> unit
