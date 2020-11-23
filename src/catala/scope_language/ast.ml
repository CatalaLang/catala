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

module Pos = Utils.Pos
module Uid = Utils.Uid
module ScopeName = Uid.Make (Uid.MarkedString)
module ScopeNameSet = Set.Make (ScopeName)
module ScopeMap = Map.Make (ScopeName)
module SubScopeName = Uid.Make (Uid.MarkedString)
module SubScopeNameSet = Set.Make (SubScopeName)
module SubScopeMap = Map.Make (SubScopeName)
module ScopeVar = Uid.Make (Uid.MarkedString)
module ScopeVarSet = Set.Make (ScopeVar)
module ScopeVarMap = Map.Make (ScopeVar)

type location = ScopeVar of ScopeVar.t | SubScopeVar of ScopeName.t * SubScopeName.t * ScopeVar.t

type expr =
  | ELocation of location
  | EVar of expr Pos.marked Bindlib.var
  | ELit of Dcalc.Ast.lit
  | EAbs of Pos.t * (expr Pos.marked, expr Pos.marked) Bindlib.binder * Dcalc.Ast.typ
  | EApp of expr Pos.marked * expr Pos.marked
  | EDefault of expr Pos.marked * expr Pos.marked * expr Pos.marked list

type rule = Definition of location * Dcalc.Ast.typ * expr | Call of ScopeName.t * SubScopeName.t

type scope_decl = { scope_decl_name : ScopeName.t; scope_decl_rules : rule list }

type program = scope_decl ScopeMap.t
