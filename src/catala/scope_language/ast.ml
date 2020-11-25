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

type location =
  | ScopeVar of ScopeVar.t Pos.marked
  | SubScopeVar of ScopeName.t * SubScopeName.t Pos.marked * ScopeVar.t Pos.marked

type expr =
  | ELocation of location
  | EVar of expr Pos.marked Bindlib.var
  | ELit of Dcalc.Ast.lit
  | EAbs of Pos.t * (expr Pos.marked, expr Pos.marked) Bindlib.mbinder * Dcalc.Ast.typ list
  | EApp of expr Pos.marked * expr Pos.marked list
  | EOp of Dcalc.Ast.operator
  | EDefault of expr Pos.marked * expr Pos.marked * expr Pos.marked list
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked

let rec locations_used (e : expr Pos.marked) : location list =
  match Pos.unmark e with
  | ELocation l -> [ l ]
  | EVar _ | ELit _ | EOp _ -> []
  | EAbs (_, binder, _) ->
      let _, body = Bindlib.unmbind binder in
      locations_used body
  | EApp (e1, args) ->
      List.fold_left (fun acc arg -> locations_used arg @ acc) (locations_used e1) args
  | EIfThenElse (e1, e2, e3) -> locations_used e1 @ locations_used e2 @ locations_used e3
  | EDefault (just, cons, subs) ->
      List.fold_left
        (fun acc sub -> locations_used sub @ acc)
        (locations_used just @ locations_used cons)
        subs

module Var = struct
  type t = expr Pos.marked Bindlib.var

  let make (s : string Pos.marked) : t =
    Bindlib.new_var (fun x -> (EVar x, Pos.get_position s)) (Pos.unmark s)

  let compare x y = Bindlib.compare_vars x y
end

type vars = expr Pos.marked Bindlib.mvar

let make_var (x : Var.t) : expr Pos.marked Bindlib.box = Bindlib.box_var x

let make_abs (xs : vars) (e : expr Pos.marked Bindlib.box) (pos_binder : Pos.t)
    (taus : Dcalc.Ast.typ list) (pos : Pos.t) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply (fun b -> (EAbs (pos_binder, b, taus), pos)) (Bindlib.bind_mvar xs e)

let make_app (e : expr Pos.marked Bindlib.box) (u : expr Pos.marked Bindlib.box list) (pos : Pos.t)
    : expr Pos.marked Bindlib.box =
  Bindlib.box_apply2 (fun e u -> (EApp (e, u), pos)) e (Bindlib.box_list u)

module VarMap = Map.Make (Var)

type rule =
  | Definition of location * Dcalc.Ast.typ * expr Pos.marked
  | Call of ScopeName.t * SubScopeName.t

type scope_decl = { scope_decl_name : ScopeName.t; scope_decl_rules : rule list }

type program = scope_decl ScopeMap.t
