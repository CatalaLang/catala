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

module StateName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module ScopeVar : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module ScopeVarSet : Set.S with type elt = ScopeVar.t = Set.Make (ScopeVar)
module ScopeVarMap : Map.S with type key = ScopeVar.t = Map.Make (ScopeVar)

(** Inside a scope, a definition can refer either to a scope def, or a subscope
    def *)
module ScopeDef = struct
  type t =
    | Var of ScopeVar.t * StateName.t option
    | SubScopeVar of Scopelang.Ast.SubScopeName.t * ScopeVar.t
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
      let cmp = Scopelang.Ast.SubScopeName.compare x' y' in
      if cmp = 0 then ScopeVar.compare x y else cmp

  let get_position x =
    match x with
    | Var (x, None) -> Marked.get_mark (ScopeVar.get_info x)
    | Var (_, Some sx) -> Marked.get_mark (StateName.get_info sx)
    | SubScopeVar (x, _) ->
      Marked.get_mark (Scopelang.Ast.SubScopeName.get_info x)

  let format_t fmt x =
    match x with
    | Var (v, None) -> ScopeVar.format_t fmt v
    | Var (v, Some sv) ->
      Format.fprintf fmt "%a.%a" ScopeVar.format_t v StateName.format_t sv
    | SubScopeVar (s, v) ->
      Format.fprintf fmt "%a.%a" Scopelang.Ast.SubScopeName.format_t s
        ScopeVar.format_t v

  let hash x =
    match x with
    | Var (v, None) -> ScopeVar.hash v
    | Var (v, Some sv) -> Int.logxor (ScopeVar.hash v) (StateName.hash sv)
    | SubScopeVar (w, v) ->
      Int.logxor (Scopelang.Ast.SubScopeName.hash w) (ScopeVar.hash v)
end

module ScopeDefMap : Map.S with type key = ScopeDef.t = Map.Make (ScopeDef)
module ScopeDefSet : Set.S with type elt = ScopeDef.t = Set.Make (ScopeDef)

(** {1 AST} *)

type location =
  | ScopeVar of ScopeVar.t Marked.pos * StateName.t option
  | SubScopeVar of
      Scopelang.Ast.ScopeName.t
      * Scopelang.Ast.SubScopeName.t Marked.pos
      * ScopeVar.t Marked.pos

module LocationSet : Set.S with type elt = location Marked.pos =
Set.Make (struct
  type t = location Marked.pos

  let compare x y =
    match Marked.unmark x, Marked.unmark y with
    | ScopeVar (vx, None), ScopeVar (vy, None)
    | ScopeVar (vx, Some _), ScopeVar (vy, None)
    | ScopeVar (vx, None), ScopeVar (vy, Some _) ->
      ScopeVar.compare (Marked.unmark vx) (Marked.unmark vy)
    | ScopeVar ((x, _), Some sx), ScopeVar ((y, _), Some sy) ->
      let cmp = ScopeVar.compare x y in
      if cmp = 0 then StateName.compare sx sy else cmp
    | ( SubScopeVar (_, (xsubindex, _), (xsubvar, _)),
        SubScopeVar (_, (ysubindex, _), (ysubvar, _)) ) ->
      let c = Scopelang.Ast.SubScopeName.compare xsubindex ysubindex in
      if c = 0 then ScopeVar.compare xsubvar ysubvar else c
    | ScopeVar _, SubScopeVar _ -> -1
    | SubScopeVar _, ScopeVar _ -> 1
end)

(** The expressions use the {{:https://lepigre.fr/ocaml-bindlib/} Bindlib}
    library, based on higher-order abstract syntax*)
type expr =
  | ELocation of location
  | EVar of expr Bindlib.var Marked.pos
  | EStruct of
      Scopelang.Ast.StructName.t
      * expr Marked.pos Scopelang.Ast.StructFieldMap.t
  | EStructAccess of
      expr Marked.pos
      * Scopelang.Ast.StructFieldName.t
      * Scopelang.Ast.StructName.t
  | EEnumInj of
      expr Marked.pos
      * Scopelang.Ast.EnumConstructor.t
      * Scopelang.Ast.EnumName.t
  | EMatch of
      expr Marked.pos
      * Scopelang.Ast.EnumName.t
      * expr Marked.pos Scopelang.Ast.EnumConstructorMap.t
  | ELit of Dcalc.Ast.lit
  | EAbs of
      (expr, expr Marked.pos) Bindlib.mbinder Marked.pos
      * Scopelang.Ast.typ Marked.pos list
  | EApp of expr Marked.pos * expr Marked.pos list
  | EOp of Dcalc.Ast.operator
  | EDefault of expr Marked.pos list * expr Marked.pos * expr Marked.pos
  | EIfThenElse of expr Marked.pos * expr Marked.pos * expr Marked.pos
  | EArray of expr Marked.pos list
  | ErrorOnEmpty of expr Marked.pos

module Expr = struct
  type t = expr

  (** Syntactic comparison, up to locations and alpha-renaming *)
  let rec compare e1 e2 =
    let rec list_compare cmp l1 l2 =
      (* List.compare is available from OCaml 4.12 on *)
      match l1, l2 with
      | [], [] -> 0
      | [], _ :: _ -> -1
      | _ :: _, [] -> 1
      | a1 :: l1, a2 :: l2 ->
        let c = cmp a1 a2 in
        if c <> 0 then c else list_compare cmp l1 l2
    in
    match e1, e2 with
    | ELocation _, ELocation _ -> 0
    | EVar (v1, _), EVar (v2, _) -> Bindlib.compare_vars v1 v2
    | EStruct (name1, field_map1), EStruct (name2, field_map2) -> (
      match Scopelang.Ast.StructName.compare name1 name2 with
      | 0 ->
        Scopelang.Ast.StructFieldMap.compare (Marked.compare compare) field_map1
          field_map2
      | n -> n)
    | ( EStructAccess ((e1, _), field_name1, struct_name1),
        EStructAccess ((e2, _), field_name2, struct_name2) ) -> (
      match compare e1 e2 with
      | 0 -> (
        match Scopelang.Ast.StructFieldName.compare field_name1 field_name2 with
        | 0 -> Scopelang.Ast.StructName.compare struct_name1 struct_name2
        | n -> n)
      | n -> n)
    | EEnumInj ((e1, _), cstr1, name1), EEnumInj ((e2, _), cstr2, name2) -> (
      match compare e1 e2 with
      | 0 -> (
        match Scopelang.Ast.EnumName.compare name1 name2 with
        | 0 -> Scopelang.Ast.EnumConstructor.compare cstr1 cstr2
        | n -> n)
      | n -> n)
    | EMatch ((e1, _), name1, emap1), EMatch ((e2, _), name2, emap2) -> (
      match compare e1 e2 with
      | 0 -> (
        match Scopelang.Ast.EnumName.compare name1 name2 with
        | 0 ->
          Scopelang.Ast.EnumConstructorMap.compare (Marked.compare compare)
            emap1 emap2
        | n -> n)
      | n -> n)
    | ELit l1, ELit l2 -> Stdlib.compare l1 l2
    | EAbs ((binder1, _), typs1), EAbs ((binder2, _), typs2) -> (
      match
        list_compare (Marked.compare Scopelang.Ast.Typ.compare) typs1 typs2
      with
      | 0 ->
        let _, (e1, _), (e2, _) = Bindlib.unmbind2 binder1 binder2 in
        compare e1 e2
      | n -> n)
    | EApp ((f1, _), args1), EApp ((f2, _), args2) -> (
      match compare f1 f2 with
      | 0 -> list_compare (fun (x1, _) (x2, _) -> compare x1 x2) args1 args2
      | n -> n)
    | EOp op1, EOp op2 -> Stdlib.compare op1 op2
    | ( EDefault (exs1, (just1, _), (cons1, _)),
        EDefault (exs2, (just2, _), (cons2, _)) ) -> (
      match compare just1 just2 with
      | 0 -> (
        match compare cons1 cons2 with
        | 0 -> list_compare (Marked.compare compare) exs1 exs2
        | n -> n)
      | n -> n)
    | ( EIfThenElse ((i1, _), (t1, _), (e1, _)),
        EIfThenElse ((i2, _), (t2, _), (e2, _)) ) -> (
      match compare i1 i2 with
      | 0 -> ( match compare t1 t2 with 0 -> compare e1 e2 | n -> n)
      | n -> n)
    | EArray a1, EArray a2 ->
      list_compare (fun (e1, _) (e2, _) -> compare e1 e2) a1 a2
    | ErrorOnEmpty (e1, _), ErrorOnEmpty (e2, _) -> compare e1 e2
    | ELocation _, _ -> -1
    | _, ELocation _ -> 1
    | EVar _, _ -> -1
    | _, EVar _ -> 1
    | EStruct _, _ -> -1
    | _, EStruct _ -> 1
    | EStructAccess _, _ -> -1
    | _, EStructAccess _ -> 1
    | EEnumInj _, _ -> -1
    | _, EEnumInj _ -> 1
    | EMatch _, _ -> -1
    | _, EMatch _ -> 1
    | ELit _, _ -> -1
    | _, ELit _ -> 1
    | EAbs _, _ -> -1
    | _, EAbs _ -> 1
    | EApp _, _ -> -1
    | _, EApp _ -> 1
    | EOp _, _ -> -1
    | _, EOp _ -> 1
    | EDefault _, _ -> -1
    | _, EDefault _ -> 1
    | EIfThenElse _, _ -> -1
    | _, EIfThenElse _ -> 1
    | EArray _, _ -> -1
    | _, EArray _ -> 1
end

module ExprMap = Map.Make (Expr)

module Var = struct
  type t = expr Bindlib.var

  let make (s : string Marked.pos) : t =
    Bindlib.new_var
      (fun (x : expr Bindlib.var) : expr -> EVar (x, Marked.get_mark s))
      (Marked.unmark s)

  let compare x y = Bindlib.compare_vars x y
end

type vars = expr Bindlib.mvar

type rule = {
  rule_id : RuleName.t;
  rule_just : expr Marked.pos Bindlib.box;
  rule_cons : expr Marked.pos Bindlib.box;
  rule_parameter : (Var.t * Scopelang.Ast.typ Marked.pos) option;
  rule_exception_to_rules : RuleSet.t Marked.pos;
}

module Rule = struct
  type t = rule

  (** Structural equality (otherwise, you should just compare the [rule_id]
      fields) *)
  let compare r1 r2 =
    match r1.rule_parameter, r2.rule_parameter with
    | None, None -> (
      let j1, _ = Bindlib.unbox r1.rule_just in
      let j2, _ = Bindlib.unbox r2.rule_just in
      match Expr.compare j1 j2 with
      | 0 ->
        let c1, _ = Bindlib.unbox r1.rule_cons in
        let c2, _ = Bindlib.unbox r2.rule_cons in
        Expr.compare c1 c2
      | n -> n)
    | Some (v1, (t1, _)), Some (v2, (t2, _)) -> (
      match Scopelang.Ast.Typ.compare t1 t2 with
      | 0 -> (
        let open Bindlib in
        let b1 = unbox (bind_var v1 r1.rule_just) in
        let b2 = unbox (bind_var v2 r2.rule_just) in
        let _, (j1, _), (j2, _) = unbind2 b1 b2 in
        match Expr.compare j1 j2 with
        | 0 ->
          let b1 = unbox (bind_var v1 r1.rule_cons) in
          let b2 = unbox (bind_var v2 r2.rule_cons) in
          let _, (c1, _), (c2, _) = unbind2 b1 b2 in
          Expr.compare c1 c2
        | n -> n)
      | n -> n)
    | None, Some _ -> -1
    | Some _, None -> 1
end

let empty_rule
    (pos : Pos.t)
    (have_parameter : Scopelang.Ast.typ Marked.pos option) : rule =
  {
    rule_just = Bindlib.box (ELit (Dcalc.Ast.LBool false), pos);
    rule_cons = Bindlib.box (ELit Dcalc.Ast.LEmptyError, pos);
    rule_parameter =
      (match have_parameter with
      | Some typ -> Some (Var.make ("dummy", pos), typ)
      | None -> None);
    rule_exception_to_rules = RuleSet.empty, pos;
    rule_id = RuleName.fresh ("empty", pos);
  }

let always_false_rule
    (pos : Pos.t)
    (have_parameter : Scopelang.Ast.typ Marked.pos option) : rule =
  {
    rule_just = Bindlib.box (ELit (Dcalc.Ast.LBool true), pos);
    rule_cons = Bindlib.box (ELit (Dcalc.Ast.LBool false), pos);
    rule_parameter =
      (match have_parameter with
      | Some typ -> Some (Var.make ("dummy", pos), typ)
      | None -> None);
    rule_exception_to_rules = RuleSet.empty, pos;
    rule_id = RuleName.fresh ("always_false", pos);
  }

type assertion = expr Marked.pos Bindlib.box
type variation_typ = Increasing | Decreasing
type reference_typ = Decree | Law

type meta_assertion =
  | FixedBy of reference_typ Marked.pos
  | VariesWith of unit * variation_typ Marked.pos option

type scope_def = {
  scope_def_rules : rule RuleMap.t;
  scope_def_typ : Scopelang.Ast.typ Marked.pos;
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

let rec locations_used (e : expr Marked.pos) : LocationSet.t =
  match Marked.unmark e with
  | ELocation l -> LocationSet.singleton (l, Marked.get_mark e)
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
          | ScopeVar (v, st) -> ScopeDef.Var (Marked.unmark v, st)
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

let make_var ((x, pos) : Var.t Marked.pos) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply (fun v -> v, pos) (Bindlib.box_var x)

let make_abs
    (xs : vars)
    (e : expr Marked.pos Bindlib.box)
    (pos_binder : Pos.t)
    (taus : Scopelang.Ast.typ Marked.pos list)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply
    (fun b -> EAbs ((b, pos_binder), taus), pos)
    (Bindlib.bind_mvar xs e)

let make_app
    (e : expr Marked.pos Bindlib.box)
    (u : expr Marked.pos Bindlib.box list)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply2 (fun e u -> EApp (e, u), pos) e (Bindlib.box_list u)

let make_let_in
    (x : Var.t)
    (tau : Scopelang.Ast.typ Marked.pos)
    (e1 : expr Marked.pos Bindlib.box)
    (e2 : expr Marked.pos Bindlib.box) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply2
    (fun e u -> EApp (e, u), Marked.get_mark (Bindlib.unbox e2))
    (make_abs (Array.of_list [x]) e2
       (Marked.get_mark (Bindlib.unbox e2))
       [tau]
       (Marked.get_mark (Bindlib.unbox e2)))
    (Bindlib.box_list [e1])

module VarMap = Map.Make (Var)
