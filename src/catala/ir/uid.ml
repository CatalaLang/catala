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

type t = int

let counter = ref 0

let fresh () : t =
  incr counter;
  !counter

module UidSet = Set.Make (Int)
module UidMap = Map.Make (Int)

let map_add_list (key : t) (item : 'a) (map : 'a list UidMap.t) =
  match UidMap.find_opt key map with
  | Some l -> UidMap.add key (item :: l) map
  | None -> UidMap.add key [ item ] map
