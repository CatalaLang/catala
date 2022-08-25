(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Nicolas Chataing <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Abstract syntax tree of the desugared representation *)

open Utils
open Shared_ast

(** {1 Names, Maps and Keys} *)

module IdentMap : Map.S with type key = String.t = Map.Make (String)

module RuleName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module RuleMap : Map.S with type key = RuleName.t = Map.Make (RuleName)
module RuleSet : Set.S with type elt = RuleName.t = Set.Make (RuleName)

module LabelName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module LabelMap : Map.S with type key = LabelName.t = Map.Make (LabelName)
module LabelSet : Set.S with type elt = LabelName.t = Set.Make (LabelName)

(** Inside a scope, a definition can refer either to a scope def, or a subscope
    def *)
module ScopeDef = struct
  type t =
    | Var of ScopeVar.t * StateName.t option
    | SubScopeVar of SubScopeName.t * ScopeVar.t
        (** In this case, the [ScopeVar.t] lives inside the context of the
            subscope's original declaration *)

  let compare x y =
    match x, y with
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
      let cmp = SubScopeName.compare x' y' in
      if cmp = 0 then ScopeVar.compare x y else cmp

  let get_position x =
    match x with
    | Var (x, None) -> Marked.get_mark (ScopeVar.get_info x)
    | Var (_, Some sx) -> Marked.get_mark (StateName.get_info sx)
    | SubScopeVar (x, _) -> Marked.get_mark (SubScopeName.get_info x)

  let format_t fmt x =
    match x with
    | Var (v, None) -> ScopeVar.format_t fmt v
    | Var (v, Some sv) ->
      Format.fprintf fmt "%a.%a" ScopeVar.format_t v StateName.format_t sv
    | SubScopeVar (s, v) ->
      Format.fprintf fmt "%a.%a" SubScopeName.format_t s ScopeVar.format_t v

  let hash x =
    match x with
    | Var (v, None) -> ScopeVar.hash v
    | Var (v, Some sv) -> Int.logxor (ScopeVar.hash v) (StateName.hash sv)
    | SubScopeVar (w, v) -> Int.logxor (SubScopeName.hash w) (ScopeVar.hash v)
end

module ScopeDefMap : Map.S with type key = ScopeDef.t = Map.Make (ScopeDef)
module ScopeDefSet : Set.S with type elt = ScopeDef.t = Set.Make (ScopeDef)

(** {1 AST} *)

type location = desugared glocation

module LocationSet : Set.S with type elt = location Marked.pos =
Set.Make (struct
  type t = location Marked.pos

  let compare = Expr.compare_location
end)

type expr = (desugared, Pos.t) gexpr

module ExprMap = Map.Make (struct
  type t = expr

  let compare = Expr.compare
end)

type exception_situation =
  | BaseCase
  | ExceptionToLabel of LabelName.t Marked.pos
  | ExceptionToRule of RuleName.t Marked.pos

type label_situation = ExplicitlyLabeled of LabelName.t Marked.pos | Unlabeled

type rule = {
  rule_id : RuleName.t;
  rule_just : expr Bindlib.box;
  rule_cons : expr Bindlib.box;
  rule_parameter : (expr Var.t * typ) option;
  rule_exception : exception_situation;
  rule_label : label_situation;
}

module Rule = struct
  type t = rule

  (** Structural equality (otherwise, you should just compare the [rule_id]
      fields) *)
  let compare r1 r2 =
    match r1.rule_parameter, r2.rule_parameter with
    | None, None -> (
      let j1 = Bindlib.unbox r1.rule_just in
      let j2 = Bindlib.unbox r2.rule_just in
      match Expr.compare j1 j2 with
      | 0 ->
        let c1 = Bindlib.unbox r1.rule_cons in
        let c2 = Bindlib.unbox r2.rule_cons in
        Expr.compare c1 c2
      | n -> n)
    | Some (v1, t1), Some (v2, t2) -> (
      match Shared_ast.Expr.compare_typ t1 t2 with
      | 0 -> (
        let open Bindlib in
        let b1 = unbox (bind_var v1 r1.rule_just) in
        let b2 = unbox (bind_var v2 r2.rule_just) in
        let _, j1, j2 = unbind2 b1 b2 in
        match Expr.compare j1 j2 with
        | 0 ->
          let b1 = unbox (bind_var v1 r1.rule_cons) in
          let b2 = unbox (bind_var v2 r2.rule_cons) in
          let _, c1, c2 = unbind2 b1 b2 in
          Expr.compare c1 c2
        | n -> n)
      | n -> n)
    | None, Some _ -> -1
    | Some _, None -> 1
end

let empty_rule (pos : Pos.t) (have_parameter : typ option) : rule =
  {
    rule_just = Bindlib.box (ELit (LBool false), pos);
    rule_cons = Bindlib.box (ELit LEmptyError, pos);
    rule_parameter =
      (match have_parameter with
      | Some typ -> Some (Var.make "dummy", typ)
      | None -> None);
    rule_exception = BaseCase;
    rule_id = RuleName.fresh ("empty", pos);
    rule_label = Unlabeled;
  }

let always_false_rule (pos : Pos.t) (have_parameter : typ option) : rule =
  {
    rule_just = Bindlib.box (ELit (LBool true), pos);
    rule_cons = Bindlib.box (ELit (LBool false), pos);
    rule_parameter =
      (match have_parameter with
      | Some typ -> Some (Var.make "dummy", typ)
      | None -> None);
    rule_exception = BaseCase;
    rule_id = RuleName.fresh ("always_false", pos);
    rule_label = Unlabeled;
  }

type assertion = expr Bindlib.box
type variation_typ = Increasing | Decreasing
type reference_typ = Decree | Law

type meta_assertion =
  | FixedBy of reference_typ Marked.pos
  | VariesWith of unit * variation_typ Marked.pos option

type scope_def = {
  scope_def_rules : rule RuleMap.t;
  scope_def_typ : typ;
  scope_def_is_condition : bool;
  scope_def_io : Scopelang.Ast.io;
}

type var_or_states = WholeVar | States of StateName.t list

type scope = {
  scope_vars : var_or_states ScopeVarMap.t;
  scope_sub_scopes : ScopeName.t Scopelang.Ast.SubScopeMap.t;
  scope_uid : ScopeName.t;
  scope_defs : scope_def ScopeDefMap.t;
  scope_assertions : assertion list;
  scope_meta_assertions : meta_assertion list;
}

type program = {
  program_scopes : scope Scopelang.Ast.ScopeMap.t;
  program_ctx : decl_ctx;
}

let rec locations_used (e : expr) : LocationSet.t =
  match Marked.unmark e with
  | ELocation l -> LocationSet.singleton (l, Marked.get_mark e)
  | EVar _ | ELit _ | EOp _ -> LocationSet.empty
  | EAbs (binder, _) ->
    let _, body = Bindlib.unmbind binder in
    locations_used body
  | EStruct (_, es) ->
    StructFieldMap.fold
      (fun _ e' acc -> LocationSet.union acc (locations_used e'))
      es LocationSet.empty
  | EStructAccess (e1, _, _) -> locations_used e1
  | EEnumInj (e1, _, _) -> locations_used e1
  | EMatchS (e1, _, es) ->
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
    List.fold_left
      (fun acc e' -> LocationSet.union acc (locations_used e'))
      LocationSet.empty es
  | ErrorOnEmpty e' -> locations_used e'

let free_variables (def : rule RuleMap.t) : Pos.t ScopeDefMap.t =
  let add_locs (acc : Pos.t ScopeDefMap.t) (locs : LocationSet.t) :
      Pos.t ScopeDefMap.t =
    LocationSet.fold
      (fun (loc, loc_pos) acc ->
        ScopeDefMap.add
          (match loc with
          | DesugaredScopeVar (v, st) -> ScopeDef.Var (Marked.unmark v, st)
          | SubScopeVar (_, sub_index, sub_var) ->
            ScopeDef.SubScopeVar (Marked.unmark sub_index, Marked.unmark sub_var))
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
