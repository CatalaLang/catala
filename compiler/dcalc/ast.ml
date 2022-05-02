(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

[@@@ocaml.warning "-7-34"]

open Utils

module ScopeName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module StructName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module StructFieldName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module StructMap : Map.S with type key = StructName.t = Map.Make (StructName)

module EnumName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module EnumConstructor : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module EnumMap : Map.S with type key = EnumName.t = Map.Make (EnumName)

type typ_lit = TBool | TUnit | TInt | TRat | TMoney | TDate | TDuration
type struct_name = StructName.t
type enum_name = EnumName.t

type typ =
  | TLit of typ_lit
  | TTuple of typ Pos.marked list * struct_name option
  | TEnum of typ Pos.marked list * enum_name
  | TArrow of typ Pos.marked * typ Pos.marked
  | TArray of typ Pos.marked
  | TAny

type date = Runtime.date
type duration = Runtime.duration
type integer = Runtime.integer
type decimal = Runtime.decimal
type money = Runtime.money

type lit =
  | LBool of bool
  | LEmptyError
  | LInt of integer
  | LRat of decimal
  | LMoney of money
  | LUnit
  | LDate of date
  | LDuration of duration

type op_kind = KInt | KRat | KMoney | KDate | KDuration
type ternop = Fold

type binop =
  | And
  | Or
  | Xor
  | Add of op_kind
  | Sub of op_kind
  | Mult of op_kind
  | Div of op_kind
  | Lt of op_kind
  | Lte of op_kind
  | Gt of op_kind
  | Gte of op_kind
  | Eq
  | Neq
  | Map
  | Concat
  | Filter

type log_entry = VarDef of typ | BeginCall | EndCall | PosRecordIfTrueBool

type unop =
  | Not
  | Minus of op_kind
  | Log of log_entry * Utils.Uid.MarkedString.info list
  | Length
  | IntToRat
  | GetDay
  | GetMonth
  | GetYear
  | RoundMoney
  | RoundDecimal

type operator = Ternop of ternop | Binop of binop | Unop of unop

type expr =
  | EVar of expr Bindlib.var Pos.marked
  | ETuple of expr Pos.marked list * struct_name option
  | ETupleAccess of
      expr Pos.marked * int * struct_name option * typ Pos.marked list
  | EInj of expr Pos.marked * int * enum_name * typ Pos.marked list
  | EMatch of expr Pos.marked * expr Pos.marked list * enum_name
  | EArray of expr Pos.marked list
  | ELit of lit
  | EAbs of
      (expr, expr Pos.marked) Bindlib.mbinder Pos.marked * typ Pos.marked list
  | EApp of expr Pos.marked * expr Pos.marked list
  | EAssert of expr Pos.marked
  | EOp of operator
  | EDefault of expr Pos.marked list * expr Pos.marked * expr Pos.marked
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked
  | ErrorOnEmpty of expr Pos.marked

type struct_ctx = (StructFieldName.t * typ Pos.marked) list StructMap.t
type enum_ctx = (EnumConstructor.t * typ Pos.marked) list EnumMap.t
type decl_ctx = { ctx_enums : enum_ctx; ctx_structs : struct_ctx }
type binder = (expr, expr Pos.marked) Bindlib.binder

type scope_let_kind =
  | DestructuringInputStruct
  | ScopeVarDefinition
  | SubScopeVarDefinition
  | CallingSubScope
  | DestructuringSubScopeResults
  | Assertion

type scope_let = {
  scope_let_kind : scope_let_kind;
  scope_let_typ : typ Utils.Pos.marked;
  scope_let_expr : expr Utils.Pos.marked;
  scope_let_next : (expr, scope_body_expr) Bindlib.binder;
  scope_let_pos : Utils.Pos.t;
}

and scope_body_expr = Result of expr Utils.Pos.marked | ScopeLet of scope_let

type scope_body = {
  scope_body_input_struct : StructName.t;
  scope_body_output_struct : StructName.t;
  scope_body_expr : (expr, scope_body_expr) Bindlib.binder;
}

type scope_def = {
  scope_name : ScopeName.t;
  scope_body : scope_body;
  scope_next : (expr, scopes) Bindlib.binder;
}

and scopes = Nil | ScopeDef of scope_def

type program = { decl_ctx : decl_ctx; scopes : scopes }

let rec fold_scope_lets
    ~(f : 'a -> scope_let -> 'a)
    ~(init : 'a)
    (scope_body_expr : scope_body_expr) : 'a =
  match scope_body_expr with
  | Result _ -> init
  | ScopeLet scope_let ->
      let _, next = Bindlib.unbind scope_let.scope_let_next in
      fold_scope_lets ~f ~init:(f init scope_let) next

let rec fold_scope_defs
    ~(f : 'a -> scope_def -> 'a) ~(init : 'a) (scopes : scopes) : 'a =
  match scopes with
  | Nil -> init
  | ScopeDef scope_def ->
      let _, next = Bindlib.unbind scope_def.scope_next in
      fold_scope_defs ~f ~init:(f init scope_def) next

module Var = struct
  type t = expr Bindlib.var

  let make (s : string Pos.marked) : t =
    Bindlib.new_var
      (fun (x : expr Bindlib.var) : expr -> EVar (x, Pos.get_position s))
      (Pos.unmark s)

  let compare x y = Bindlib.compare_vars x y
end

(** See [Bindlib.box_term] documentation for why we are doing that. *)
let rec box_expr (e : expr Pos.marked) : expr Pos.marked Bindlib.box =
  match Pos.unmark e with
  | EVar (v, _pos) ->
      Bindlib.box_apply (fun v -> (v, Pos.get_position e)) (Bindlib.box_var v)
  | EApp (f, args) ->
      Bindlib.box_apply2
        (fun f args -> (EApp (f, args), Pos.get_position e))
        (box_expr f)
        (Bindlib.box_list (List.map box_expr args))
  | EAbs ((binder, binder_pos), typs) ->
      Bindlib.box_apply
        (fun binder -> (EAbs ((binder, binder_pos), typs), Pos.get_position e))
        (Bindlib.box_mbinder box_expr binder)
  | ETuple (args, s) ->
      Bindlib.box_apply
        (fun args -> (ETuple (args, s), Pos.get_position e))
        (Bindlib.box_list (List.map box_expr args))
  | ETupleAccess (e1, n, s_name, typs) ->
      Bindlib.box_apply
        (fun e1 -> (ETupleAccess (e1, n, s_name, typs), Pos.get_position e))
        (box_expr e1)
  | EInj (e1, i, e_name, typ) ->
      Bindlib.box_apply
        (fun e1 -> (EInj (e1, i, e_name, typ), Pos.get_position e))
        (box_expr e1)
  | EMatch (arg, arms, e_name) ->
      Bindlib.box_apply2
        (fun arg arms -> (EMatch (arg, arms, e_name), Pos.get_position e))
        (box_expr arg)
        (Bindlib.box_list (List.map box_expr arms))
  | EArray args ->
      Bindlib.box_apply
        (fun args -> (EArray args, Pos.get_position e))
        (Bindlib.box_list (List.map box_expr args))
  | ELit l -> Bindlib.box (ELit l, Pos.get_position e)
  | EAssert e1 ->
      Bindlib.box_apply
        (fun e1 -> (EAssert e1, Pos.get_position e))
        (box_expr e1)
  | EOp op -> Bindlib.box (EOp op, Pos.get_position e)
  | EDefault (excepts, just, cons) ->
      Bindlib.box_apply3
        (fun excepts just cons ->
          (EDefault (excepts, just, cons), Pos.get_position e))
        (Bindlib.box_list (List.map box_expr excepts))
        (box_expr just) (box_expr cons)
  | EIfThenElse (e1, e2, e3) ->
      Bindlib.box_apply3
        (fun e1 e2 e3 -> (EIfThenElse (e1, e2, e3), Pos.get_position e))
        (box_expr e1) (box_expr e2) (box_expr e3)
  | ErrorOnEmpty e1 ->
      Bindlib.box_apply
        (fun e1 -> (ErrorOnEmpty e1, Pos.get_position e1))
        (box_expr e1)

module VarMap = Map.Make (Var)
module VarSet = Set.Make (Var)

let rec free_vars_expr (e : expr Pos.marked) : VarSet.t =
  match Pos.unmark e with
  | EVar (v, _) -> VarSet.singleton v
  | ETuple (es, _) | EArray es ->
      es |> List.map free_vars_expr |> List.fold_left VarSet.union VarSet.empty
  | ETupleAccess (e1, _, _, _)
  | EAssert e1
  | ErrorOnEmpty e1
  | EInj (e1, _, _, _) ->
      free_vars_expr e1
  | EApp (e1, es) | EMatch (e1, es, _) ->
      e1 :: es |> List.map free_vars_expr
      |> List.fold_left VarSet.union VarSet.empty
  | EDefault (es, ejust, econs) ->
      ejust :: econs :: es |> List.map free_vars_expr
      |> List.fold_left VarSet.union VarSet.empty
  | EOp _ | ELit _ -> VarSet.empty
  | EIfThenElse (e1, e2, e3) ->
      [ e1; e2; e3 ] |> List.map free_vars_expr
      |> List.fold_left VarSet.union VarSet.empty
  | EAbs ((binder, _), _) ->
      let vs, body = Bindlib.unmbind binder in
      Array.fold_right VarSet.remove vs (free_vars_expr body)

let rec free_vars_scope_body_expr (scope_lets : scope_body_expr) : VarSet.t =
  match scope_lets with
  | Result e -> free_vars_expr e
  | ScopeLet { scope_let_expr = e; scope_let_next = next; _ } ->
      let v, body = Bindlib.unbind next in
      VarSet.union (free_vars_expr e)
        (VarSet.remove v (free_vars_scope_body_expr body))

let free_vars_scope_body (scope_body : scope_body) : VarSet.t =
  let { scope_body_expr = binder; _ } = scope_body in
  let v, body = Bindlib.unbind binder in
  VarSet.remove v (free_vars_scope_body_expr body)

let rec free_vars_scopes (scopes : scopes) : VarSet.t =
  match scopes with
  | Nil -> VarSet.empty
  | ScopeDef { scope_body = body; scope_next = next; _ } ->
      let v, next = Bindlib.unbind next in
      VarSet.union
        (VarSet.remove v (free_vars_scopes next))
        (free_vars_scope_body body)

type vars = expr Bindlib.mvar

let make_var ((x, pos) : Var.t Pos.marked) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply (fun x -> (x, pos)) (Bindlib.box_var x)

let make_abs
    (xs : vars)
    (e : expr Pos.marked Bindlib.box)
    (pos_binder : Pos.t)
    (taus : typ Pos.marked list)
    (pos : Pos.t) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply
    (fun b -> (EAbs ((b, pos_binder), taus), pos))
    (Bindlib.bind_mvar xs e)

let make_app
    (e : expr Pos.marked Bindlib.box)
    (u : expr Pos.marked Bindlib.box list)
    (pos : Pos.t) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply2 (fun e u -> (EApp (e, u), pos)) e (Bindlib.box_list u)

let make_let_in
    (x : Var.t)
    (tau : typ Pos.marked)
    (e1 : expr Pos.marked Bindlib.box)
    (e2 : expr Pos.marked Bindlib.box)
    (pos : Pos.t) : expr Pos.marked Bindlib.box =
  make_app (make_abs (Array.of_list [ x ]) e2 pos [ tau ] pos) [ e1 ] pos

let empty_thunked_term : expr Pos.marked =
  let silent = Var.make ("_", Pos.no_pos) in
  Bindlib.unbox
    (make_abs
       (Array.of_list [ silent ])
       (Bindlib.box (ELit LEmptyError, Pos.no_pos))
       Pos.no_pos
       [ (TLit TUnit, Pos.no_pos) ]
       Pos.no_pos)

let is_value (e : expr Pos.marked) : bool =
  match Pos.unmark e with ELit _ | EAbs _ | EOp _ -> true | _ -> false

let rec equal_typs (ty1 : typ Pos.marked) (ty2 : typ Pos.marked) : bool =
  match (Pos.unmark ty1, Pos.unmark ty2) with
  | TLit l1, TLit l2 -> l1 = l2
  | TTuple (tys1, n1), TTuple (tys2, n2) -> n1 = n2 && equal_typs_list tys1 tys2
  | TEnum (tys1, n1), TEnum (tys2, n2) -> n1 = n2 && equal_typs_list tys1 tys2
  | TArrow (t1, t1'), TArrow (t2, t2') -> equal_typs t1 t2 && equal_typs t1' t2'
  | TArray t1, TArray t2 -> equal_typs t1 t2
  | TAny, TAny -> true
  | _, _ -> false

and equal_typs_list (tys1 : typ Pos.marked list) (tys2 : typ Pos.marked list) :
    bool =
  List.length tys1 = List.length tys2
  && (* OCaml && operator short-circuits when a clause is false, we can safely
        assume here that both lists have equal length *)
  List.for_all (fun (x, y) -> equal_typs x y) (List.combine tys1 tys2)

let equal_log_entries (l1 : log_entry) (l2 : log_entry) : bool =
  match (l1, l2) with
  | VarDef t1, VarDef t2 -> equal_typs (t1, Pos.no_pos) (t2, Pos.no_pos)
  | x, y -> x = y

let equal_unops (op1 : unop) (op2 : unop) : bool =
  match (op1, op2) with
  (* Log entries contain a typ which contain position information, we thus need
     to descend into them *)
  | Log (l1, info1), Log (l2, info2) -> equal_log_entries l1 l2 && info1 = info2
  (* All the other cases can be discharged through equality *)
  | _ -> op1 = op2

let equal_ops (op1 : operator) (op2 : operator) : bool =
  match (op1, op2) with
  | Ternop op1, Ternop op2 -> op1 = op2
  | Binop op1, Binop op2 -> op1 = op2
  | Unop op1, Unop op2 -> equal_unops op1 op2
  | _, _ -> false

let rec equal_exprs (e1 : expr Pos.marked) (e2 : expr Pos.marked) : bool =
  match (Pos.unmark e1, Pos.unmark e2) with
  | EVar v1, EVar v2 -> Pos.unmark v1 = Pos.unmark v2
  | ETuple (es1, n1), ETuple (es2, n2) -> n1 = n2 && equal_exprs_list es1 es2
  | ETupleAccess (e1, id1, n1, tys1), ETupleAccess (e2, id2, n2, tys2) ->
      equal_exprs e1 e2 && id1 = id2 && n1 = n2 && equal_typs_list tys1 tys2
  | EInj (e1, id1, n1, tys1), EInj (e2, id2, n2, tys2) ->
      equal_exprs e1 e2 && id1 = id2 && n1 = n2 && equal_typs_list tys1 tys2
  | EMatch (e1, cases1, n1), EMatch (e2, cases2, n2) ->
      n1 = n2 && equal_exprs e1 e2 && equal_exprs_list cases1 cases2
  | EArray es1, EArray es2 -> equal_exprs_list es1 es2
  | ELit l1, ELit l2 -> l1 = l2
  | EAbs (b1, tys1), EAbs (b2, tys2) ->
      equal_typs_list tys1 tys2
      &&
      let vars1, body1 = Bindlib.unmbind (Pos.unmark b1) in
      let body2 =
        Bindlib.msubst (Pos.unmark b2)
          (Array.map (fun x -> EVar (x, Pos.no_pos)) vars1)
      in
      equal_exprs body1 body2
  | EAssert e1, EAssert e2 -> equal_exprs e1 e2
  | EOp op1, EOp op2 -> equal_ops op1 op2
  | EDefault (exc1, def1, cons1), EDefault (exc2, def2, cons2) ->
      equal_exprs def1 def2 && equal_exprs cons1 cons2
      && equal_exprs_list exc1 exc2
  | EIfThenElse (if1, then1, else1), EIfThenElse (if2, then2, else2) ->
      equal_exprs if1 if2 && equal_exprs then1 then2 && equal_exprs else1 else2
  | ErrorOnEmpty e1, ErrorOnEmpty e2 -> equal_exprs e1 e2
  | _, _ -> false

and equal_exprs_list (es1 : expr Pos.marked list) (es2 : expr Pos.marked list) :
    bool =
  List.length es1 = List.length es2
  && (* OCaml && operator short-circuits when a clause is false, we can safely
        assume here that both lists have equal length *)
  List.for_all (fun (x, y) -> equal_exprs x y) (List.combine es1 es2)

let rec unfold_scope_body_expr (ctx : decl_ctx) (scope_let : scope_body_expr) :
    expr Pos.marked Bindlib.box =
  match scope_let with
  | Result e -> box_expr e
  | ScopeLet
      {
        scope_let_kind = _;
        scope_let_typ;
        scope_let_expr;
        scope_let_next;
        scope_let_pos;
      } ->
      let var, next = Bindlib.unbind scope_let_next in
      make_let_in var scope_let_typ (box_expr scope_let_expr)
        (unfold_scope_body_expr ctx next)
        scope_let_pos

let build_whole_scope_expr
    (ctx : decl_ctx) (body : scope_body) (pos_scope : Pos.t) =
  let var, body_expr = Bindlib.unbind body.scope_body_expr in
  let body_expr = unfold_scope_body_expr ctx body_expr in
  make_abs
    (Array.of_list [ var ])
    body_expr pos_scope
    [
      ( TTuple
          ( List.map snd
              (StructMap.find body.scope_body_input_struct ctx.ctx_structs),
            Some body.scope_body_input_struct ),
        pos_scope );
    ]
    pos_scope

let build_scope_typ_from_sig
    (ctx : decl_ctx)
    (scope_input_struct_name : StructName.t)
    (scope_return_struct_name : StructName.t)
    (pos : Pos.t) : typ Pos.marked =
  let scope_sig = StructMap.find scope_input_struct_name ctx.ctx_structs in
  let scope_return_typ =
    StructMap.find scope_return_struct_name ctx.ctx_structs
  in
  let result_typ =
    (TTuple (List.map snd scope_return_typ, Some scope_return_struct_name), pos)
  in
  let input_typ =
    (TTuple (List.map snd scope_sig, Some scope_input_struct_name), pos)
  in
  (TArrow (input_typ, result_typ), pos)

type scope_name_or_var = ScopeName of ScopeName.t | ScopeVar of Var.t

let rec unfold_scopes
    (ctx : decl_ctx) (s : scopes) (main_scope : scope_name_or_var) :
    expr Pos.marked Bindlib.box =
  match s with
  | Nil -> (
      match main_scope with
      | ScopeVar v ->
          Bindlib.box_apply (fun v -> (v, Pos.no_pos)) (Bindlib.box_var v)
      | ScopeName _ -> failwith "should not happen")
  | ScopeDef { scope_name; scope_body; scope_next } ->
      let scope_var, scope_next = Bindlib.unbind scope_next in
      let scope_pos = Pos.get_position (ScopeName.get_info scope_name) in
      let main_scope =
        match main_scope with
        | ScopeVar v -> ScopeVar v
        | ScopeName n ->
            if ScopeName.compare n scope_name = 0 then ScopeVar scope_var
            else ScopeName n
      in
      make_let_in scope_var
        (build_scope_typ_from_sig ctx scope_body.scope_body_input_struct
           scope_body.scope_body_output_struct scope_pos)
        (build_whole_scope_expr ctx scope_body scope_pos)
        (unfold_scopes ctx scope_next main_scope)
        scope_pos

let build_whole_program_expr (p : program) (main_scope : ScopeName.t) =
  unfold_scopes p.decl_ctx p.scopes (ScopeName main_scope)

let rec expr_size (e : expr Pos.marked) : int =
  match Pos.unmark e with
  | EVar _ | ELit _ | EOp _ -> 1
  | ETuple (args, _) | EArray args ->
      List.fold_left (fun acc arg -> acc + expr_size arg) 1 args
  | ETupleAccess (e1, _, _, _)
  | EInj (e1, _, _, _)
  | EAssert e1
  | ErrorOnEmpty e1 ->
      expr_size e1 + 1
  | EMatch (arg, args, _) | EApp (arg, args) ->
      List.fold_left
        (fun acc arg -> acc + expr_size arg)
        (1 + expr_size arg)
        args
  | EAbs ((binder, _), _) ->
      let _, body = Bindlib.unmbind binder in
      1 + expr_size body
  | EIfThenElse (e1, e2, e3) -> 1 + expr_size e1 + expr_size e2 + expr_size e3
  | EDefault (exceptions, just, cons) ->
      List.fold_left
        (fun acc except -> acc + expr_size except)
        (1 + expr_size just + expr_size cons)
        exceptions
