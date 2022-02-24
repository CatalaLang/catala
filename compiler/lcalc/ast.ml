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
module D = Dcalc.Ast

type lit =
  | LBool of bool
  | LInt of Runtime.integer
  | LRat of Runtime.decimal
  | LMoney of Runtime.money
  | LUnit
  | LDate of Runtime.date
  | LDuration of Runtime.duration

type except = ConflictError | EmptyError | NoValueProvided | Crash

type expr =
  | EVar of expr Bindlib.var Pos.marked
  | ETuple of expr Pos.marked list * D.StructName.t option
      (** The [MarkedString.info] is the former struct field name*)
  | ETupleAccess of expr Pos.marked * int * D.StructName.t option * D.typ Pos.marked list
      (** The [MarkedString.info] is the former struct field name *)
  | EInj of expr Pos.marked * int * D.EnumName.t * D.typ Pos.marked list
      (** The [MarkedString.info] is the former enum case name *)
  | EMatch of expr Pos.marked * expr Pos.marked list * D.EnumName.t
      (** The [MarkedString.info] is the former enum case name *)
  | EArray of expr Pos.marked list
  | ELit of lit
  | EAbs of (expr, expr Pos.marked) Bindlib.mbinder Pos.marked * D.typ Pos.marked list
  | EApp of expr Pos.marked * expr Pos.marked list
  | EAssert of expr Pos.marked
  | EOp of D.operator
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked
  | ERaise of except
  | ECatch of expr Pos.marked * except * expr Pos.marked

module Var = struct
  type t = expr Bindlib.var

  let make (s : string Pos.marked) : t =
    Bindlib.new_var
      (fun (x : expr Bindlib.var) : expr -> EVar (x, Pos.get_position s))
      (Pos.unmark s)

  let compare x y = Bindlib.compare_vars x y
end

module VarMap = Map.Make (Var)

type vars = expr Bindlib.mvar

let make_var ((x, pos) : Var.t Pos.marked) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply (fun x -> (x, pos)) (Bindlib.box_var x)

let make_abs (xs : vars) (e : expr Pos.marked Bindlib.box) (pos_binder : Pos.t)
    (taus : D.typ Pos.marked list) (pos : Pos.t) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply (fun b -> (EAbs ((b, pos_binder), taus), pos)) (Bindlib.bind_mvar xs e)

let make_app (e : expr Pos.marked Bindlib.box) (u : expr Pos.marked Bindlib.box list) (pos : Pos.t)
    : expr Pos.marked Bindlib.box =
  Bindlib.box_apply2 (fun e u -> (EApp (e, u), pos)) e (Bindlib.box_list u)

let make_let_in (x : Var.t) (tau : D.typ Pos.marked) (e1 : expr Pos.marked Bindlib.box)
    (e2 : expr Pos.marked Bindlib.box) : expr Pos.marked Bindlib.box =
  let pos = Pos.get_position (Bindlib.unbox e2) in

  make_app (make_abs (Array.of_list [ x ]) e2 pos [ tau ] pos) [ e1 ] pos

let ( let+ ) x f = Bindlib.box_apply f x

let ( and+ ) x y = Bindlib.box_pair x y

let option_enum : D.EnumName.t = D.EnumName.fresh ("eoption", Pos.no_pos)

let none_constr : D.EnumConstructor.t = D.EnumConstructor.fresh ("ENone", Pos.no_pos)

let some_constr : D.EnumConstructor.t = D.EnumConstructor.fresh ("ESome", Pos.no_pos)

let option_enum_config : (D.EnumConstructor.t * D.typ Pos.marked) list =
  [ (none_constr, (D.TLit D.TUnit, Pos.no_pos)); (some_constr, (D.TAny, Pos.no_pos)) ]

let make_none (pos : Pos.t) : expr Pos.marked Bindlib.box =
  let mark : 'a -> 'a Pos.marked = Pos.mark pos in
  Bindlib.box @@ mark
  @@ EInj (mark @@ ELit LUnit, 0, option_enum, [ (D.TLit D.TUnit, pos); (D.TAny, pos) ])

let make_some (e : expr Pos.marked Bindlib.box) : expr Pos.marked Bindlib.box =
  let pos = Pos.get_position @@ Bindlib.unbox e in
  let mark : 'a -> 'a Pos.marked = Pos.mark pos in
  let+ e = e in
  mark @@ EInj (e, 1, option_enum, [ (D.TLit D.TUnit, pos); (D.TAny, pos) ])

(** [make_matchopt_with_abs_arms arg e_none e_some] build an expression
    [match arg with |None -> e_none | Some -> e_some] and requires e_some and e_none to be in the
    form [EAbs ...].*)
let make_matchopt_with_abs_arms (arg : expr Pos.marked Bindlib.box)
    (e_none : expr Pos.marked Bindlib.box) (e_some : expr Pos.marked Bindlib.box) :
    expr Pos.marked Bindlib.box =
  let pos = Pos.get_position @@ Bindlib.unbox arg in
  let mark : 'a -> 'a Pos.marked = Pos.mark pos in

  let+ arg = arg and+ e_none = e_none and+ e_some = e_some in

  mark @@ EMatch (arg, [ e_none; e_some ], option_enum)

(** [make_matchopt pos v tau arg e_none e_some] builds an expression
    [match arg with | None () -> e_none | Some v -> e_some]. It binds v to e_some, permitting it to
    be used inside the expression. There is no requirements on the form of both e_some and e_none. *)
let make_matchopt (pos : Pos.t) (v : Var.t) (tau : D.typ Pos.marked)
    (arg : expr Pos.marked Bindlib.box) (e_none : expr Pos.marked Bindlib.box)
    (e_some : expr Pos.marked Bindlib.box) : expr Pos.marked Bindlib.box =
  let x = Var.make ("_", pos) in

  make_matchopt_with_abs_arms arg
    (make_abs (Array.of_list [ x ]) e_none pos [ (D.TLit D.TUnit, pos) ] pos)
    (make_abs (Array.of_list [ v ]) e_some pos [ tau ] pos)

let handle_default = Var.make ("handle_default", Pos.no_pos)

let handle_default_opt = Var.make ("handle_default_opt", Pos.no_pos)

type binder = (expr, expr Pos.marked) Bindlib.binder

type scope_body = {
  scope_body_name : Dcalc.Ast.ScopeName.t;
  scope_body_var : Var.t;
  scope_body_expr : expr Pos.marked;
}

type program = { decl_ctx : D.decl_ctx; scopes : scope_body list }
