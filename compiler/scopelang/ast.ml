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

open Utils
module ScopeName = Dcalc.Ast.ScopeName

module ScopeNameSet : Set.S with type elt = ScopeName.t = Set.Make (ScopeName)

module ScopeMap : Map.S with type key = ScopeName.t = Map.Make (ScopeName)

module SubScopeName : Uid.Id with type info = Uid.MarkedString.info = Uid.Make (Uid.MarkedString) ()

module SubScopeNameSet : Set.S with type elt = SubScopeName.t = Set.Make (SubScopeName)

module SubScopeMap : Map.S with type key = SubScopeName.t = Map.Make (SubScopeName)

module ScopeVar : Uid.Id with type info = Uid.MarkedString.info = Uid.Make (Uid.MarkedString) ()

module ScopeVarSet : Set.S with type elt = ScopeVar.t = Set.Make (ScopeVar)

module ScopeVarMap : Map.S with type key = ScopeVar.t = Map.Make (ScopeVar)

module StructName = Dcalc.Ast.StructName
module StructMap = Dcalc.Ast.StructMap
module StructFieldName = Dcalc.Ast.StructFieldName

module StructFieldMap : Map.S with type key = StructFieldName.t = Map.Make (StructFieldName)

module EnumName = Dcalc.Ast.EnumName
module EnumMap = Dcalc.Ast.EnumMap
module EnumConstructor = Dcalc.Ast.EnumConstructor

module EnumConstructorMap : Map.S with type key = EnumConstructor.t = Map.Make (EnumConstructor)

type location =
  | ScopeVar of ScopeVar.t Pos.marked
  | SubScopeVar of ScopeName.t * SubScopeName.t Pos.marked * ScopeVar.t Pos.marked

module LocationSet : Set.S with type elt = location Pos.marked = Set.Make (struct
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

type typ =
  | TLit of Dcalc.Ast.typ_lit
  | TStruct of StructName.t
  | TEnum of EnumName.t
  | TArrow of typ Pos.marked * typ Pos.marked
  | TArray of typ
  | TAny

type expr =
  | ELocation of location
  | EVar of expr Bindlib.var Pos.marked
  | EStruct of StructName.t * expr Pos.marked StructFieldMap.t
  | EStructAccess of expr Pos.marked * StructFieldName.t * StructName.t
  | EEnumInj of expr Pos.marked * EnumConstructor.t * EnumName.t
  | EMatch of expr Pos.marked * EnumName.t * expr Pos.marked EnumConstructorMap.t
  | ELit of Dcalc.Ast.lit
  | EAbs of (expr, expr Pos.marked) Bindlib.mbinder Pos.marked * typ Pos.marked list
  | EApp of expr Pos.marked * expr Pos.marked list
  | EOp of Dcalc.Ast.operator
  | EDefault of expr Pos.marked list * expr Pos.marked * expr Pos.marked
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked
  | EArray of expr Pos.marked list
  | ErrorOnEmpty of expr Pos.marked

let rec locations_used (e : expr Pos.marked) : LocationSet.t =
  match Pos.unmark e with
  | ELocation l -> LocationSet.singleton (l, Pos.get_position e)
  | EVar _ | ELit _ | EOp _ -> LocationSet.empty
  | EAbs ((binder, _), _) ->
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
  | EDefault (excepts, just, cons) ->
      List.fold_left
        (fun acc except -> LocationSet.union (locations_used except) acc)
        (LocationSet.union (locations_used just) (locations_used cons))
        excepts
  | EArray es ->
      List.fold_left (fun acc e' -> LocationSet.union acc (locations_used e')) LocationSet.empty es
  | ErrorOnEmpty e' -> locations_used e'

type visibility = { visibility_output : bool; visibility_input : bool }

type rule =
  | Definition of location Pos.marked * typ Pos.marked * expr Pos.marked
  | Assertion of expr Pos.marked
  | Call of ScopeName.t * SubScopeName.t

type scope_decl = {
  scope_decl_name : ScopeName.t;
  scope_sig : (typ Pos.marked * visibility) ScopeVarMap.t;
  scope_decl_rules : rule list;
}

type struct_ctx = (StructFieldName.t * typ Pos.marked) list StructMap.t

type enum_ctx = (EnumConstructor.t * typ Pos.marked) list EnumMap.t

type program = {
  program_scopes : scope_decl ScopeMap.t;
  program_enums : enum_ctx;
  program_structs : struct_ctx;
}

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
    (taus : typ Pos.marked list) (pos : Pos.t) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply (fun b -> (EAbs ((b, pos_binder), taus), pos)) (Bindlib.bind_mvar xs e)

let make_app (e : expr Pos.marked Bindlib.box) (u : expr Pos.marked Bindlib.box list) (pos : Pos.t)
    : expr Pos.marked Bindlib.box =
  Bindlib.box_apply2 (fun e u -> (EApp (e, u), pos)) e (Bindlib.box_list u)

let make_let_in (x : Var.t) (tau : typ Pos.marked) (e1 : expr Pos.marked Bindlib.box)
    (e2 : expr Pos.marked Bindlib.box) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply2
    (fun e u -> (EApp (e, u), Pos.get_position (Bindlib.unbox e2)))
    (make_abs
       (Array.of_list [ x ])
       e2
       (Pos.get_position (Bindlib.unbox e2))
       [ tau ]
       (Pos.get_position (Bindlib.unbox e2)))
    (Bindlib.box_list [ e1 ])

module VarMap = Map.Make (Var)
