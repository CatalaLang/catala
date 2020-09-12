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

module Make (X : sig
  type info
end) =
struct
  type t = { id : int; info : X.info }

  module UidSet = Set.Make (Int)
  module UidMap = Map.Make (Int)

  let ident_tbl = ref UidMap.empty

  let counter = ref 0

  let fresh (info : X.info) : t =
    incr counter;
    ident_tbl := UidMap.add !counter info !ident_tbl;
    { id = !counter; info }

  let get_info (uid : t) : X.info = UidMap.find uid.id !ident_tbl

  let map_add_list (key : t) (item : 'a) (map : 'a list UidMap.t) =
    match UidMap.find_opt key.id map with
    | Some l -> UidMap.add key.id (item :: l) map
    | None -> UidMap.add key.id [ item ] map

  let compare (x : t) (y : t) : int = compare x.id y.id
end

module MarkedString = struct
  type info = string Pos.marked
end

module Scope = Make (MarkedString)
module ScopeSet = Set.Make (Scope)
module ScopeMap = Map.Make (Scope)
module Var = Make (MarkedString)
module VarSet = Set.Make (Var)
module VarMap = Map.Make (Var)
module LocalVar = Make (MarkedString)
module LocalVarSet = Set.Make (LocalVar)
module LocalVarMap = Map.Make (LocalVar)
module SubScope = Make (MarkedString)
module SubScopeSet = Set.Make (SubScope)
module SubScopeMap = Map.Make (SubScope)
