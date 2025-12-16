(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Florian Angeletti
   <florian.angeletti@inria.fr>, Vincent Botbol <vincent.botbol@inria.fr>

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
open Definitions

module FileMap : Map.S with type key = File.t
(** More efficient File map module *)

(** More efficient ScopeName set module *)
module ScopeSet : sig
  include Set.S with type elt = ScopeName.t

  val same_scope : ScopeName.t -> ScopeName.t -> bool
  (** Tests whether two scopes points the same location instead of comparing
      their's ids *)
end

(** A position can either be [Unreached] or [Reached_by] a set of test scopes. *)
type cover = Unreached | Reached_by of { scopes : ScopeSet.t }

val format_cover : Format.formatter -> cover -> unit

type itv = private {
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

val from_pos : Pos.t -> itv
val format_itv : Format.formatter -> itv -> unit

type coverage_map

val empty : coverage_map

val reached_pos : Pos.t -> ScopeName.t -> coverage_map -> coverage_map
(** [reached_pos p s m] marks the position [p] as reached by the scope [s] in
    the map [m]. Merge scopes if the position was already reached by another
    scope. *)

val unreached_pos : Pos.t -> coverage_map -> coverage_map
(** [unreached_pos p m] marks the position [p] as unreached in the map [m]. Does
    not overwrite the existing cover. *)

val reachable_positions : _ gexpr program -> coverage_map
(** [reachable_positions prg] iterates over the program [p]'s expressions to
    build a [Unreached] only [coverage_map]. *)

val merge_with_reachable_positions :
  reachable:coverage_map -> reached:coverage_map -> coverage_map
(** [merge_with_reachable_positions ~reachable ~reached] computes the union
    between [reachable] and [reached] but filter out files that do not contain
    reached positions. When a conflict occurs, the cover are merged, i.e.,
    scopes are merged together. *)

val filter_files : (File.t -> bool) -> coverage_map -> coverage_map
(** [filter_files p m] removes the files from the coverage map that do not
    satisfy the predicate [p]. Used to filter out the stdlib. *)

val union : coverage_map -> coverage_map -> coverage_map
val fold : (File.t * itv -> cover -> 'a -> 'a) -> coverage_map -> 'a -> 'a

val format_coverage_hex_dump : Format.formatter -> coverage_map -> unit
(** Marshal the coverage map and prints it as hexadecimal. *)

val of_hex : string -> coverage_map
(** Unmarshal the coverage map from an hexadecimal string. *)

type interval_tree = interval_node list
(** n-ary tree that modelize the coverage of a file. Every node's sub-trees are
    comprised in their parent wrt. their interval. This is the structure that
    should be exported. *)

and interval_node = { itv : itv; cover : cover; children : interval_tree }

val compute_interval_trees : coverage_map -> interval_tree FileMap.t
(** Build and normalize a coverage map into an interval tree. *)

val all_scopes : interval_tree -> ScopeSet.t
(** [all_scopes itvt] iterates over the interval tree [itvt] and returns the
    union of all scopes that produced a [Reached_by] cover. *)

val format_interval_tree : Format.formatter -> interval_tree -> unit
