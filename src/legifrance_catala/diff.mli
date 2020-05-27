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

(** Simple and inefficient diff algorithm based on longest common subsequences *)

(** The diff algorithm works on comparable items *)
module type Comparable = sig
  type t

  val compare : t -> t -> int
end

(** Functor that produces a [Diff] module given a comparable type *)
module Make : functor (X : Comparable) -> sig
  type item = X.t

  type diff = Deleted of item list | Added of item list | Equal of item list

  type t = diff list

  val get_diff : item array -> item array -> t
  (** This is the main function : [get_diff a1 a2] compares two arrays of items and outputs a list
      of chunks tagged with [Deteted], [Added] or [Removed] *)
end
