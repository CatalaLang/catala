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

(** Abstract syntax tree of the desugared representation *)

open Utils

(** {1 Names, Maps and Keys} *)

module IdentMap : Map.S with type key = String.t = Map.Make (String)

module RuleName : Uid.Id with type info = Uid.MarkedString.info = Uid.Make (Uid.MarkedString) ()

module RuleMap : Map.S with type key = RuleName.t = Map.Make (RuleName)

module RuleSet : Set.S with type elt = RuleName.t = Set.Make (RuleName)

module LabelName : Uid.Id with type info = Uid.MarkedString.info = Uid.Make (Uid.MarkedString) ()

module LabelMap : Map.S with type key = LabelName.t = Map.Make (LabelName)

module LabelSet : Set.S with type elt = LabelName.t = Set.Make (LabelName)

module StateName : Uid.Id with type info = Uid.MarkedString.info = Uid.Make (Uid.MarkedString) ()

module ScopeVar : Uid.Id with type info = Uid.MarkedString.info = Uid.Make (Uid.MarkedString) ()

module ScopeVarSet : Set.S with type elt = ScopeVar.t = Set.Make (ScopeVar)

module ScopeVarMap : Map.S with type key = ScopeVar.t = Map.Make (ScopeVar)

(** Inside a scope, a definition can refer either to a scope def, or a subscope def *)
module ScopeDef = struct
  type t =
    | Var of ScopeVar.t * StateName.t option
    | SubScopeVar of Scopelang.Ast.SubScopeName.t * ScopeVar.t
        (** In this case, the [ScopeVar.t] lives inside the context of the subscope's original
            declaration *)

  let compare x y =
    match (x, y) with
    | Var (x, None), Var (y, None)
    | Var (x, Some _), Var (y, None)
    | Var (x, None), Var (y, Some _)
    | Var (x, _), SubScopeVar (_, y)
    | SubScopeVar (_, x), Var (y, _) ->
        ScopeVar.compare x y
    | Var (x, Some sx), Var (y, Some sy) ->
        let cmp = ScopeVar.compare x y in
        if cmp = 0 then StateName.compare sx sy else cmp
    | SubScopeVar (x', x), SubScopeVar (y', y) ->
        let cmp = Scopelang.Ast.SubScopeName.compare x' y' in
        if cmp = 0 then ScopeVar.compare x y else cmp

  let get_position x =
    match x with
    | Var (x, None) -> Pos.get_position (ScopeVar.get_info x)
    | Var (_, Some sx) -> Pos.get_position (StateName.get_info sx)
    | SubScopeVar (x, _) -> Pos.get_position (Scopelang.Ast.SubScopeName.get_info x)

  let format_t fmt x =
    match x with
    | Var (v, None) -> ScopeVar.format_t fmt v
    | Var (v, Some sv) -> Format.fprintf fmt "%a.%a" ScopeVar.format_t v StateName.format_t sv
    | SubScopeVar (s, v) ->
        Format.fprintf fmt "%a.%a" Scopelang.Ast.SubScopeName.format_t s ScopeVar.format_t v

  let hash x =
    match x with
    | Var (v, None) -> ScopeVar.hash v
    | Var (v, Some sv) -> Int.logxor (ScopeVar.hash v) (StateName.hash sv)
    | SubScopeVar (w, v) -> Int.logxor (Scopelang.Ast.SubScopeName.hash w) (ScopeVar.hash v)
end

module ScopeDefMap : Map.S with type key = ScopeDef.t = Map.Make (ScopeDef)

module ScopeDefSet : Set.S with type elt = ScopeDef.t = Set.Make (ScopeDef)

(** {1 AST} *)

type location =
  | ScopeVar of ScopeVar.t Pos.marked * StateName.t option
  | SubScopeVar of
      Scopelang.Ast.ScopeName.t * Scopelang.Ast.SubScopeName.t Pos.marked * ScopeVar.t Pos.marked

module LocationSet : Set.S with type elt = location Pos.marked = Set.Make (struct
  type t = location Pos.marked

  let compare x y =
    match (Pos.unmark x, Pos.unmark y) with
    | ScopeVar (vx, None), ScopeVar (vy, None)
    | ScopeVar (vx, Some _), ScopeVar (vy, None)
    | ScopeVar (vx, None), ScopeVar (vy, Some _) ->
        ScopeVar.compare (Pos.unmark vx) (Pos.unmark vy)
    | ScopeVar ((x, _), Some sx), ScopeVar ((y, _), Some sy) ->
        let cmp = ScopeVar.compare x y in
        if cmp = 0 then StateName.compare sx sy else cmp
    | SubScopeVar (_, (xsubindex, _), (xsubvar, _)), SubScopeVar (_, (ysubindex, _), (ysubvar, _))
      ->
        let c = Scopelang.Ast.SubScopeName.compare xsubindex ysubindex in
        if c = 0 then ScopeVar.compare xsubvar ysubvar else c
    | ScopeVar _, SubScopeVar _ -> -1
    | SubScopeVar _, ScopeVar _ -> 1
end)

(** The expressions use the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib} library, based on
    higher-order abstract syntax*)
type expr =
  | ELocation of location
  | EVar of expr Bindlib.var Pos.marked
  | EStruct of Scopelang.Ast.StructName.t * expr Pos.marked Scopelang.Ast.StructFieldMap.t
  | EStructAccess of expr Pos.marked * Scopelang.Ast.StructFieldName.t * Scopelang.Ast.StructName.t
  | EEnumInj of expr Pos.marked * Scopelang.Ast.EnumConstructor.t * Scopelang.Ast.EnumName.t
  | EMatch of
      expr Pos.marked
      * Scopelang.Ast.EnumName.t
      * expr Pos.marked Scopelang.Ast.EnumConstructorMap.t
  | ELit of Dcalc.Ast.lit
  | EAbs of (expr, expr Pos.marked) Bindlib.mbinder Pos.marked * Scopelang.Ast.typ Pos.marked list
  | EApp of expr Pos.marked * expr Pos.marked list
  | EOp of Dcalc.Ast.operator
  | EDefault of expr Pos.marked list * expr Pos.marked * expr Pos.marked
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked
  | EArray of expr Pos.marked list
  | ErrorOnEmpty of expr Pos.marked

module Var = struct
  type t = expr Bindlib.var

  let make (s : string Pos.marked) : t =
    Bindlib.new_var
      (fun (x : expr Bindlib.var) : expr -> EVar (x, Pos.get_position s))
      (Pos.unmark s)

  let compare x y = Bindlib.compare_vars x y
end

type vars = expr Bindlib.mvar

type rule = {
  rule_id : RuleName.t;
  rule_just : expr Pos.marked Bindlib.box;
  rule_cons : expr Pos.marked Bindlib.box;
  rule_parameter : (Var.t * Scopelang.Ast.typ Pos.marked) option;
  rule_exception_to_rules : RuleSet.t Pos.marked;
}

let empty_rule (pos : Pos.t) (have_parameter : Scopelang.Ast.typ Pos.marked option) : rule =
  {
    rule_just = Bindlib.box (ELit (Dcalc.Ast.LBool false), pos);
    rule_cons = Bindlib.box (ELit Dcalc.Ast.LEmptyError, pos);
    rule_parameter =
      (match have_parameter with Some typ -> Some (Var.make ("dummy", pos), typ) | None -> None);
    rule_exception_to_rules = (RuleSet.empty, pos);
    rule_id = RuleName.fresh ("empty", pos);
  }

let always_false_rule (pos : Pos.t) (have_parameter : Scopelang.Ast.typ Pos.marked option) : rule =
  {
    rule_just = Bindlib.box (ELit (Dcalc.Ast.LBool true), pos);
    rule_cons = Bindlib.box (ELit (Dcalc.Ast.LBool false), pos);
    rule_parameter =
      (match have_parameter with Some typ -> Some (Var.make ("dummy", pos), typ) | None -> None);
    rule_exception_to_rules = (RuleSet.empty, pos);
    rule_id = RuleName.fresh ("always_false", pos);
  }

type assertion = expr Pos.marked Bindlib.box

type variation_typ = Increasing | Decreasing

type reference_typ = Decree | Law

type meta_assertion =
  | FixedBy of reference_typ Pos.marked
  | VariesWith of unit * variation_typ Pos.marked option

type scope_def = {
  scope_def_rules : rule RuleMap.t;
  scope_def_typ : Scopelang.Ast.typ Pos.marked;
  scope_def_is_condition : bool;
  scope_def_io : Scopelang.Ast.io;
  scope_def_label_groups : RuleSet.t LabelMap.t;
}

type var_or_states = WholeVar | States of StateName.t list

type scope = {
  scope_vars : var_or_states ScopeVarMap.t;
  scope_sub_scopes : Scopelang.Ast.ScopeName.t Scopelang.Ast.SubScopeMap.t;
  scope_uid : Scopelang.Ast.ScopeName.t;
  scope_defs : scope_def ScopeDefMap.t;
  scope_assertions : assertion list;
  scope_meta_assertions : meta_assertion list;
}

type program = {
  program_scopes : scope Scopelang.Ast.ScopeMap.t;
  program_enums : Scopelang.Ast.enum_ctx;
  program_structs : Scopelang.Ast.struct_ctx;
}

let rec locations_used (e : expr Pos.marked) : LocationSet.t =
  match Pos.unmark e with
  | ELocation l -> LocationSet.singleton (l, Pos.get_position e)
  | EVar _ | ELit _ | EOp _ -> LocationSet.empty
  | EAbs ((binder, _), _) ->
      let _, body = Bindlib.unmbind binder in
      locations_used body
  | EStruct (_, es) ->
      Scopelang.Ast.StructFieldMap.fold
        (fun _ e' acc -> LocationSet.union acc (locations_used e'))
        es LocationSet.empty
  | EStructAccess (e1, _, _) -> locations_used e1
  | EEnumInj (e1, _, _) -> locations_used e1
  | EMatch (e1, _, es) ->
      Scopelang.Ast.EnumConstructorMap.fold
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

let free_variables (def : rule RuleMap.t) : Pos.t ScopeDefMap.t =
  let add_locs (acc : Pos.t ScopeDefMap.t) (locs : LocationSet.t) : Pos.t ScopeDefMap.t =
    LocationSet.fold
      (fun (loc, loc_pos) acc ->
        ScopeDefMap.add
          (match loc with
          | ScopeVar (v, st) -> ScopeDef.Var (Pos.unmark v, st)
          | SubScopeVar (_, sub_index, sub_var) ->
              ScopeDef.SubScopeVar (Pos.unmark sub_index, Pos.unmark sub_var))
          loc_pos acc)
      locs acc
  in
  RuleMap.fold
    (fun _ rule acc ->
      let locs =
        LocationSet.union
          (locations_used (Bindlib.unbox rule.rule_just))
          (locations_used (Bindlib.unbox rule.rule_cons))
      in
      add_locs acc locs)
    def ScopeDefMap.empty

let make_var ((x, pos) : Var.t Pos.marked) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply (fun v -> (v, pos)) (Bindlib.box_var x)

let make_abs (xs : vars) (e : expr Pos.marked Bindlib.box) (pos_binder : Pos.t)
    (taus : Scopelang.Ast.typ Pos.marked list) (pos : Pos.t) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply (fun b -> (EAbs ((b, pos_binder), taus), pos)) (Bindlib.bind_mvar xs e)

let make_app (e : expr Pos.marked Bindlib.box) (u : expr Pos.marked Bindlib.box list) (pos : Pos.t)
    : expr Pos.marked Bindlib.box =
  Bindlib.box_apply2 (fun e u -> (EApp (e, u), pos)) e (Bindlib.box_list u)

let make_let_in (x : Var.t) (tau : Scopelang.Ast.typ Pos.marked) (e1 : expr Pos.marked Bindlib.box)
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
