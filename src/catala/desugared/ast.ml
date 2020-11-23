(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

module Pos = Utils.Pos
module Uid = Utils.Uid
module IdentMap = Map.Make (String)
module Var = Uid.Make (Uid.MarkedString)
module VarSet = Set.Make (Var)
module VarMap = Map.Make (Var)
module LocalVar = Uid.Make (Uid.MarkedString)
module LocalVarSet = Set.Make (LocalVar)
module LocalVarMap = Map.Make (LocalVar)

(** Inside a scope, a definition can refer either to a scope def, or a subscope def *)
module ScopeDef = struct
  type t =
    | Var of Var.t
    | SubScopeVar of Scopelang.Ast.SubScopeName.t * Var.t
        (** In this case, the [Uid.Var.t] lives inside the context of the subscope's original
            declaration *)

  let compare x y =
    match (x, y) with
    | Var x, Var y | Var x, SubScopeVar (_, y) | SubScopeVar (_, x), Var y -> Var.compare x y
    | SubScopeVar (_, x), SubScopeVar (_, y) -> Scopelang.Ast.SubScopeName.compare x y

  let format_t x =
    match x with
    | Var v -> Var.format_t v
    | SubScopeVar (s, v) ->
        Printf.sprintf "%s.%s" (Scopelang.Ast.SubScopeName.format_t s) (Var.format_t v)

  let hash x = match x with Var v -> Var.hash v | SubScopeVar (_, v) -> Var.hash v
end

module ScopeDefMap = Map.Make (ScopeDef)
module ScopeDefSet = Set.Make (ScopeDef)

(* Scopes *)
type binder = LocalVar.t

type definition = unit

let empty_def (_pos : Pos.t) (_typ : Dcalc.Ast.typ) : definition = assert false

type assertion = unit

type variation_typ = Increasing | Decreasing

type reference_typ = Decree | Law

type meta_assertion =
  | FixedBy of reference_typ Pos.marked
  | VariesWith of unit * variation_typ Pos.marked option

type scope = {
  scope_uid : Scopelang.Ast.ScopeName.t;
  scope_defs : definition ScopeDefMap.t;
  scope_assertions : assertion list;
  scope_meta_assertions : meta_assertion list;
}

let empty_scope (uid : Scopelang.Ast.ScopeName.t) : scope =
  {
    scope_uid = uid;
    scope_defs = ScopeDefMap.empty;
    scope_assertions = [];
    scope_meta_assertions = [];
  }

type program = scope Scopelang.Ast.ScopeMap.t
