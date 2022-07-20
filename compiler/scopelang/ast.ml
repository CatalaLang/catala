(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Utils
module ScopeName = Dcalc.Ast.ScopeName
module ScopeNameSet : Set.S with type elt = ScopeName.t = Set.Make (ScopeName)
module ScopeMap : Map.S with type key = ScopeName.t = Map.Make (ScopeName)

module SubScopeName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module SubScopeNameSet : Set.S with type elt = SubScopeName.t =
  Set.Make (SubScopeName)

module SubScopeMap : Map.S with type key = SubScopeName.t =
  Map.Make (SubScopeName)

module ScopeVar : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module ScopeVarSet : Set.S with type elt = ScopeVar.t = Set.Make (ScopeVar)
module ScopeVarMap : Map.S with type key = ScopeVar.t = Map.Make (ScopeVar)
module StructName = Dcalc.Ast.StructName
module StructMap = Dcalc.Ast.StructMap
module StructFieldName = Dcalc.Ast.StructFieldName

module StructFieldMap : Map.S with type key = StructFieldName.t =
  Map.Make (StructFieldName)

module StructFieldMapLift = Bindlib.Lift (StructFieldMap)
module EnumName = Dcalc.Ast.EnumName
module EnumMap = Dcalc.Ast.EnumMap
module EnumConstructor = Dcalc.Ast.EnumConstructor

module EnumConstructorMap : Map.S with type key = EnumConstructor.t =
  Map.Make (EnumConstructor)

module EnumConstructorMapLift = Bindlib.Lift (EnumConstructorMap)

type location =
  | ScopeVar of ScopeVar.t Marked.pos
  | SubScopeVar of
      ScopeName.t * SubScopeName.t Marked.pos * ScopeVar.t Marked.pos

module LocationSet : Set.S with type elt = location Marked.pos =
Set.Make (struct
  type t = location Marked.pos

  let compare x y =
    match Marked.unmark x, Marked.unmark y with
    | ScopeVar (vx, _), ScopeVar (vy, _) -> ScopeVar.compare vx vy
    | ( SubScopeVar (_, (xsubindex, _), (xsubvar, _)),
        SubScopeVar (_, (ysubindex, _), (ysubvar, _)) ) ->
      let c = SubScopeName.compare xsubindex ysubindex in
      if c = 0 then ScopeVar.compare xsubvar ysubvar else c
    | ScopeVar _, SubScopeVar _ -> -1
    | SubScopeVar _, ScopeVar _ -> 1
end)

type typ =
  | TLit of Dcalc.Ast.typ_lit
  | TStruct of StructName.t
  | TEnum of EnumName.t
  | TArrow of typ Marked.pos * typ Marked.pos
  | TArray of typ
  | TAny

module Typ = struct
  type t = typ

  let rec compare ty1 ty2 =
    match ty1, ty2 with
    | TLit l1, TLit l2 -> Stdlib.compare l1 l2
    | TStruct n1, TStruct n2 -> StructName.compare n1 n2
    | TEnum en1, TEnum en2 -> EnumName.compare en1 en2
    | TArrow ((a1, _), (b1, _)), TArrow ((a2, _), (b2, _)) -> (
      match compare a1 a2 with 0 -> compare b1 b2 | n -> n)
    | TArray t1, TArray t2 -> compare t1 t2
    | TAny, TAny -> 0
    | TLit _, _ -> -1
    | _, TLit _ -> 1
    | TStruct _, _ -> -1
    | _, TStruct _ -> 1
    | TEnum _, _ -> -1
    | _, TEnum _ -> 1
    | TArrow _, _ -> -1
    | _, TArrow _ -> 1
    | TArray _, _ -> -1
    | _, TArray _ -> 1
end

type marked_expr = expr Marked.pos

and expr =
  | ELocation of location
  | EVar of expr Bindlib.var
  | EStruct of StructName.t * marked_expr StructFieldMap.t
  | EStructAccess of marked_expr * StructFieldName.t * StructName.t
  | EEnumInj of marked_expr * EnumConstructor.t * EnumName.t
  | EMatch of marked_expr * EnumName.t * marked_expr EnumConstructorMap.t
  | ELit of Dcalc.Ast.lit
  | EAbs of (expr, marked_expr) Bindlib.mbinder * typ Marked.pos list
  | EApp of marked_expr * marked_expr list
  | EOp of Dcalc.Ast.operator
  | EDefault of marked_expr list * marked_expr * marked_expr
  | EIfThenElse of marked_expr * marked_expr * marked_expr
  | EArray of marked_expr list
  | ErrorOnEmpty of marked_expr

module Expr = struct
  type t = expr

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
    | EVar v1, EVar v2 -> Bindlib.compare_vars v1 v2
    | EStruct (name1, field_map1), EStruct (name2, field_map2) -> (
      match StructName.compare name1 name2 with
      | 0 ->
        StructFieldMap.compare (Marked.compare compare) field_map1 field_map2
      | n -> n)
    | ( EStructAccess ((e1, _), field_name1, struct_name1),
        EStructAccess ((e2, _), field_name2, struct_name2) ) -> (
      match compare e1 e2 with
      | 0 -> (
        match StructFieldName.compare field_name1 field_name2 with
        | 0 -> StructName.compare struct_name1 struct_name2
        | n -> n)
      | n -> n)
    | EEnumInj ((e1, _), cstr1, name1), EEnumInj ((e2, _), cstr2, name2) -> (
      match compare e1 e2 with
      | 0 -> (
        match EnumName.compare name1 name2 with
        | 0 -> EnumConstructor.compare cstr1 cstr2
        | n -> n)
      | n -> n)
    | EMatch ((e1, _), name1, emap1), EMatch ((e2, _), name2, emap2) -> (
      match compare e1 e2 with
      | 0 -> (
        match EnumName.compare name1 name2 with
        | 0 -> EnumConstructorMap.compare (Marked.compare compare) emap1 emap2
        | n -> n)
      | n -> n)
    | ELit l1, ELit l2 -> Stdlib.compare l1 l2
    | EAbs (binder1, typs1), EAbs (binder2, typs2) -> (
      match list_compare (Marked.compare Typ.compare) typs1 typs2 with
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

let rec locations_used (e : expr Marked.pos) : LocationSet.t =
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
    List.fold_left
      (fun acc e' -> LocationSet.union acc (locations_used e'))
      LocationSet.empty es
  | ErrorOnEmpty e' -> locations_used e'

type io_input = NoInput | OnlyInput | Reentrant
type io = { io_output : bool Marked.pos; io_input : io_input Marked.pos }

type rule =
  | Definition of location Marked.pos * typ Marked.pos * io * expr Marked.pos
  | Assertion of expr Marked.pos
  | Call of ScopeName.t * SubScopeName.t

type scope_decl = {
  scope_decl_name : ScopeName.t;
  scope_sig : (typ Marked.pos * io) ScopeVarMap.t;
  scope_decl_rules : rule list;
}

type struct_ctx = (StructFieldName.t * typ Marked.pos) list StructMap.t
type enum_ctx = (EnumConstructor.t * typ Marked.pos) list EnumMap.t

type program = {
  program_scopes : scope_decl ScopeMap.t;
  program_enums : enum_ctx;
  program_structs : struct_ctx;
}

module Var = struct
  type t = expr Bindlib.var

  let make (s : string) : t =
    Bindlib.new_var (fun (x : expr Bindlib.var) : expr -> EVar x) s

  let compare x y = Bindlib.compare_vars x y
end

type vars = expr Bindlib.mvar

let make_var ((x, pos) : Var.t Marked.pos) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply (fun v -> v, pos) (Bindlib.box_var x)

let make_abs
    (xs : vars)
    (e : expr Marked.pos Bindlib.box)
    (taus : typ Marked.pos list)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply (fun b -> EAbs (b, taus), pos) (Bindlib.bind_mvar xs e)

let make_app
    (e : expr Marked.pos Bindlib.box)
    (u : expr Marked.pos Bindlib.box list)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply2 (fun e u -> EApp (e, u), pos) e (Bindlib.box_list u)

let make_let_in
    (x : Var.t)
    (tau : typ Marked.pos)
    (e1 : expr Marked.pos Bindlib.box)
    (e2 : expr Marked.pos Bindlib.box) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply2
    (fun e u -> EApp (e, u), Marked.get_mark (Bindlib.unbox e2))
    (make_abs (Array.of_list [x]) e2 [tau] (Marked.get_mark (Bindlib.unbox e2)))
    (Bindlib.box_list [e1])

let make_default ?(pos = Pos.no_pos) exceptions just cons =
  let rec bool_value = function
    | ELit (Dcalc.Ast.LBool b), _ -> Some b
    | EApp ((EOp (Unop (Log (l, _))), _), [e]), _
      when l <> Dcalc.Ast.PosRecordIfTrueBool
           (* we don't remove the log calls corresponding to source code
              definitions !*) ->
      bool_value e
    | _ -> None
  in
  match exceptions, bool_value just, cons with
  | [], Some true, cons -> cons
  | exceptions, Some true, (EDefault ([], just, cons), pos) ->
    EDefault (exceptions, just, cons), pos
  | [except], Some false, _ -> except
  | exceptions, _, cons ->
    let pos = if pos <> Pos.no_pos then pos else Marked.get_mark just in
    EDefault (exceptions, just, cons), pos

module VarMap = Map.Make (Var)
