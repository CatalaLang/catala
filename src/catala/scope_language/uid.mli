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

module IdentMap : Map.S with type key = String.t

module MarkedString : sig
  type info = string Pos.marked

  val format_info : 'a * 'b -> 'a
end

module Scope : sig
  type t

  val fresh : MarkedString.info -> t

  val get_info : t -> MarkedString.info

  val compare : t -> t -> int

  val format_t : t -> string
end

module ScopeSet : Set.S with type elt = Scope.t

module ScopeMap : Map.S with type key = Scope.t

module Var : sig
  type t

  val fresh : MarkedString.info -> t

  val get_info : t -> MarkedString.info

  val compare : t -> t -> int

  val format_t : t -> string
end

module VarSet : Set.S with type elt = Var.t

module VarMap : Map.S with type key = Var.t

module LocalVar : sig
  type t

  val fresh : MarkedString.info -> t

  val get_info : t -> MarkedString.info

  val compare : t -> t -> int

  val format_t : t -> string
end

module LocalVarSet : Set.S with type elt = LocalVar.t

module LocalVarMap : Map.S with type key = LocalVar.t

module SubScope : sig
  type t

  val fresh : MarkedString.info -> t

  val get_info : t -> MarkedString.info

  val compare : t -> t -> int

  val format_t : t -> string
end

module SubScopeSet : Set.S with type elt = SubScope.t

module SubScopeMap : Map.S with type key = SubScope.t

module ScopeDef : sig
  type t = Var of Var.t | SubScopeVar of SubScope.t * Var.t

  val compare : t -> t -> int

  val format_t : t -> string
end

module ScopeDefMap : Map.S with type key = ScopeDef.t
