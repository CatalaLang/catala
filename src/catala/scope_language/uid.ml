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

module IdentMap = Map.Make (String)

module Make (X : sig
  type info

  val format_info : info -> string
end) =
struct
  type t = { id : int; info : X.info }

  let counter = ref 0

  let fresh (info : X.info) : t =
    incr counter;
    { id = !counter; info }

  let get_info (uid : t) : X.info = uid.info

  let compare (x : t) (y : t) : int = compare x.id y.id

  let format_t (x : t) : string = Printf.sprintf "%s[%d]" (X.format_info x.info) x.id
end

module MarkedString = struct
  type info = string Pos.marked

  let format_info (s, _) = s
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

(** Inside a scope, a definition can refer either to a scope def, or a subscope def *)
module ScopeDef = struct
  type t =
    | Var of Var.t
    | SubScopeVar of SubScope.t * Var.t
        (** In this case, the [Uid.Var.t] lives inside the context of the subscope's original
            declaration *)

  let compare x y =
    match (x, y) with
    | Var x, Var y
    | Var x, SubScopeVar (_, y)
    | SubScopeVar (_, x), Var y
    | SubScopeVar (_, x), SubScopeVar (_, y) ->
        compare x.id y.id

  let format_t x =
    match x with
    | Var v -> Var.format_t v
    | SubScopeVar (s, v) -> Printf.sprintf "%s.%s" (SubScope.format_t s) (Var.format_t v)
end

module ScopeDefMap = Map.Make (ScopeDef)
