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

module ScopeName = Uid.Make (Uid.MarkedString) ()

module ScopeNameSet = Set.Make (ScopeName)
module ScopeMap = Map.Make (ScopeName)

module SubScopeName = Uid.Make (Uid.MarkedString) ()

module SubScopeNameSet = Set.Make (SubScopeName)
module SubScopeMap = Map.Make (SubScopeName)

module ScopeVar = Uid.Make (Uid.MarkedString) ()

module ScopeVarSet = Set.Make (ScopeVar)
module ScopeVarMap = Map.Make (ScopeVar)

module StructName = Uid.Make (Uid.MarkedString) ()

module StructMap = Map.Make (StructName)

module StructFieldName = Uid.Make (Uid.MarkedString) ()

module StructFieldMap = Map.Make (StructFieldName)

module EnumName = Uid.Make (Uid.MarkedString) ()

module EnumMap = Map.Make (EnumName)

module EnumConstructor = Uid.Make (Uid.MarkedString) ()

module EnumConstructorMap = Map.Make (EnumConstructor)

type location =
  | ScopeVar of ScopeVar.t Pos.marked
  | SubScopeVar of ScopeName.t * SubScopeName.t Pos.marked * ScopeVar.t Pos.marked

module LocationSet = Set.Make (struct
  type t = location Pos.marked

  let compare x y =
    match (Pos.unmark x, Pos.unmark y) with
    | ScopeVar (vx, _), ScopeVar (vy, _) -> ScopeVar.compare vx vy
    | SubScopeVar (_, (xsubindex, _), (xsubvar, _)), SubScopeVar (_, (ysubindex, _), (ysubvar, _))
      ->
        let c = SubScopeName.compare xsubindex ysubindex in
        if c = 0 then ScopeVar.compare xsubvar ysubvar else c
    | ScopeVar _, SubScopeVar _ -> -1
    | SubScopeVar _, ScopeVar _ -> 1
end)

type expr =
  | ELocation of location
  | EVar of expr Bindlib.var Pos.marked
  | EStruct of StructName.t * expr Pos.marked StructFieldMap.t
  | EStructAccess of expr Pos.marked * StructFieldName.t * StructName.t
  | EEnumInj of expr Pos.marked * EnumConstructor.t * EnumName.t
  | EMatch of expr Pos.marked * EnumName.t * expr Pos.marked EnumConstructorMap.t
  | ELit of Dcalc.Ast.lit
  | EAbs of Pos.t * (expr, expr Pos.marked) Bindlib.mbinder * Dcalc.Ast.typ Pos.marked list
  | EApp of expr Pos.marked * expr Pos.marked list
  | EOp of Dcalc.Ast.operator
  | EDefault of expr Pos.marked * expr Pos.marked * expr Pos.marked list
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked

let rec locations_used (e : expr Pos.marked) : LocationSet.t =
  match Pos.unmark e with
  | ELocation l -> LocationSet.singleton (l, Pos.get_position e)
  | EVar _ | ELit _ | EOp _ -> LocationSet.empty
  | EAbs (_, binder, _) ->
      let _, body = Bindlib.unmbind binder in
      locations_used body
  | EStruct (_, es) ->
      StructFieldMap.fold
        (fun _ e' acc -> LocationSet.union acc (locations_used e'))
        es LocationSet.empty
  | EStructAccess (e1, _, _) -> locations_used e1
  | EEnumInj (e1, _, _) -> locations_used e1
  | EMatch (e1, _, es) ->
      EnumConstructorMap.fold
        (fun _ e' acc -> LocationSet.union acc (locations_used e'))
        es (locations_used e1)
  | EApp (e1, args) ->
      List.fold_left
        (fun acc arg -> LocationSet.union (locations_used arg) acc)
        (locations_used e1) args
  | EIfThenElse (e1, e2, e3) ->
      LocationSet.union (locations_used e1)
        (LocationSet.union (locations_used e2) (locations_used e3))
  | EDefault (just, cons, subs) ->
      List.fold_left
        (fun acc sub -> LocationSet.union (locations_used sub) acc)
        (LocationSet.union (locations_used just) (locations_used cons))
        subs

module Var = struct
  type t = expr Bindlib.var

  let make (s : string Pos.marked) : t =
    Bindlib.new_var
      (fun (x : expr Bindlib.var) : expr -> EVar (x, Pos.get_position s))
      (Pos.unmark s)

  let compare x y = Bindlib.compare_vars x y
end

type vars = expr Bindlib.mvar

let make_var ((x, pos) : Var.t Pos.marked) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply (fun v -> (v, pos)) (Bindlib.box_var x)

let make_abs (xs : vars) (e : expr Pos.marked Bindlib.box) (pos_binder : Pos.t)
    (taus : Dcalc.Ast.typ Pos.marked list) (pos : Pos.t) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply (fun b -> (EAbs (pos_binder, b, taus), pos)) (Bindlib.bind_mvar xs e)

let make_app (e : expr Pos.marked Bindlib.box) (u : expr Pos.marked Bindlib.box list) (pos : Pos.t)
    : expr Pos.marked Bindlib.box =
  Bindlib.box_apply2 (fun e u -> (EApp (e, u), pos)) e (Bindlib.box_list u)

module VarMap = Map.Make (Var)

type rule =
  | Definition of location Pos.marked * Dcalc.Ast.typ Pos.marked * expr Pos.marked
  | Call of ScopeName.t * SubScopeName.t

type scope_decl = {
  scope_decl_name : ScopeName.t;
  scope_sig : Dcalc.Ast.typ Pos.marked ScopeVarMap.t;
  scope_decl_rules : rule list;
}

type program = scope_decl ScopeMap.t
