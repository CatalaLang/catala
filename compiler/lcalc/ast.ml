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
  | EVar of expr Bindlib.var Marked.pos
  | ETuple of expr Marked.pos list * D.StructName.t option
      (** The [MarkedString.info] is the former struct field name*)
  | ETupleAccess of
      expr Marked.pos * int * D.StructName.t option * D.typ Marked.pos list
      (** The [MarkedString.info] is the former struct field name *)
  | EInj of expr Marked.pos * int * D.EnumName.t * D.typ Marked.pos list
      (** The [MarkedString.info] is the former enum case name *)
  | EMatch of expr Marked.pos * expr Marked.pos list * D.EnumName.t
      (** The [MarkedString.info] is the former enum case name *)
  | EArray of expr Marked.pos list
  | ELit of lit
  | EAbs of
      (expr, expr Marked.pos) Bindlib.mbinder Marked.pos * D.typ Marked.pos list
  | EApp of expr Marked.pos * expr Marked.pos list
  | EAssert of expr Marked.pos
  | EOp of D.operator
  | EIfThenElse of expr Marked.pos * expr Marked.pos * expr Marked.pos
  | ERaise of except
  | ECatch of expr Marked.pos * except * expr Marked.pos

type program = { decl_ctx : Dcalc.Ast.decl_ctx; scopes : expr Dcalc.Ast.scopes }

let evar (v : expr Bindlib.var) (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply (fun v' -> v', pos) (Bindlib.box_var v)

let etuple
    (args : expr Marked.pos Bindlib.box list)
    (s : Dcalc.Ast.StructName.t option)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply (fun args -> ETuple (args, s), pos) (Bindlib.box_list args)

let etupleaccess
    (e1 : expr Marked.pos Bindlib.box)
    (i : int)
    (s : Dcalc.Ast.StructName.t option)
    (typs : Dcalc.Ast.typ Marked.pos list)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply (fun e1 -> ETupleAccess (e1, i, s, typs), pos) e1

let einj
    (e1 : expr Marked.pos Bindlib.box)
    (i : int)
    (e_name : Dcalc.Ast.EnumName.t)
    (typs : Dcalc.Ast.typ Marked.pos list)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply (fun e1 -> EInj (e1, i, e_name, typs), pos) e1

let ematch
    (arg : expr Marked.pos Bindlib.box)
    (arms : expr Marked.pos Bindlib.box list)
    (e_name : Dcalc.Ast.EnumName.t)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply2
    (fun arg arms -> EMatch (arg, arms, e_name), pos)
    arg (Bindlib.box_list arms)

let earray (args : expr Marked.pos Bindlib.box list) (pos : Pos.t) :
    expr Marked.pos Bindlib.box =
  Bindlib.box_apply (fun args -> EArray args, pos) (Bindlib.box_list args)

let elit (l : lit) (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box (ELit l, pos)

let eabs
    (binder : (expr, expr Marked.pos) Bindlib.mbinder Bindlib.box)
    (pos_binder : Pos.t)
    (typs : Dcalc.Ast.typ Marked.pos list)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply
    (fun binder -> EAbs ((binder, pos_binder), typs), pos)
    binder

let eapp
    (e1 : expr Marked.pos Bindlib.box)
    (args : expr Marked.pos Bindlib.box list)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply2
    (fun e1 args -> EApp (e1, args), pos)
    e1 (Bindlib.box_list args)

let eassert (e1 : expr Marked.pos Bindlib.box) (pos : Pos.t) :
    expr Marked.pos Bindlib.box =
  Bindlib.box_apply (fun e1 -> EAssert e1, pos) e1

let eop (op : Dcalc.Ast.operator) (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box (EOp op, pos)

let eraise (e1 : except) (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box (ERaise e1, pos)

let ecatch
    (e1 : expr Marked.pos Bindlib.box)
    (exn : except)
    (e2 : expr Marked.pos Bindlib.box)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply2 (fun e1 e2 -> ECatch (e1, exn, e2), pos) e1 e2

let eifthenelse
    (e1 : expr Marked.pos Bindlib.box)
    (e2 : expr Marked.pos Bindlib.box)
    (e3 : expr Marked.pos Bindlib.box)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply3 (fun e1 e2 e3 -> EIfThenElse (e1, e2, e3), pos) e1 e2 e3

module Var = struct
  type t = expr Bindlib.var

  let make (s : string Marked.pos) : t =
    Bindlib.new_var
      (fun (x : expr Bindlib.var) : expr -> EVar (x, Marked.get_mark s))
      (Marked.unmark s)

  let compare x y = Bindlib.compare_vars x y
end

module VarMap = Map.Make (Var)
module VarSet = Set.Make (Var)

type vars = expr Bindlib.mvar

let map_expr
    (ctx : 'a)
    ~(f : 'a -> expr Marked.pos -> expr Marked.pos Bindlib.box)
    (e : expr Marked.pos) : expr Marked.pos Bindlib.box =
  match Marked.unmark e with
  | EVar (v, _pos) -> evar v (Marked.get_mark e)
  | EApp (e1, args) ->
    eapp (f ctx e1) (List.map (f ctx) args) (Marked.get_mark e)
  | EAbs ((binder, binder_pos), typs) ->
    eabs
      (Bindlib.box_mbinder (f ctx) binder)
      binder_pos typs (Marked.get_mark e)
  | ETuple (args, s) -> etuple (List.map (f ctx) args) s (Marked.get_mark e)
  | ETupleAccess (e1, n, s_name, typs) ->
    etupleaccess ((f ctx) e1) n s_name typs (Marked.get_mark e)
  | EInj (e1, i, e_name, typs) ->
    einj ((f ctx) e1) i e_name typs (Marked.get_mark e)
  | EMatch (arg, arms, e_name) ->
    ematch ((f ctx) arg) (List.map (f ctx) arms) e_name (Marked.get_mark e)
  | EArray args -> earray (List.map (f ctx) args) (Marked.get_mark e)
  | ELit l -> elit l (Marked.get_mark e)
  | EAssert e1 -> eassert ((f ctx) e1) (Marked.get_mark e)
  | EOp op -> Bindlib.box (EOp op, Marked.get_mark e)
  | ERaise exn -> eraise exn (Marked.get_mark e)
  | EIfThenElse (e1, e2, e3) ->
    eifthenelse ((f ctx) e1) ((f ctx) e2) ((f ctx) e3) (Marked.get_mark e)
  | ECatch (e1, exn, e2) -> ecatch (f ctx e1) exn (f ctx e2) (Marked.get_mark e)

(** See [Bindlib.box_term] documentation for why we are doing that. *)
let box_expr (e : expr Marked.pos) : expr Marked.pos Bindlib.box =
  let rec id_t () e = map_expr () ~f:id_t e in
  id_t () e

let make_var ((x, pos) : Var.t Marked.pos) : expr Marked.pos Bindlib.box =
  Bindlib.box_apply (fun x -> x, pos) (Bindlib.box_var x)

let make_abs
    (xs : vars)
    (e : expr Marked.pos Bindlib.box)
    (pos_binder : Pos.t)
    (taus : D.typ Marked.pos list)
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
    (tau : D.typ Marked.pos)
    (e1 : expr Marked.pos Bindlib.box)
    (e2 : expr Marked.pos Bindlib.box)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  make_app (make_abs (Array.of_list [x]) e2 pos [tau] pos) [e1] pos

let make_multiple_let_in
    (xs : Var.t array)
    (taus : D.typ Marked.pos list)
    (e1 : expr Marked.pos Bindlib.box list)
    (e2 : expr Marked.pos Bindlib.box)
    (pos : Pos.t) : expr Marked.pos Bindlib.box =
  make_app (make_abs xs e2 pos taus pos) e1 pos

let ( let+ ) x f = Bindlib.box_apply f x
let ( and+ ) x y = Bindlib.box_pair x y
let option_enum : D.EnumName.t = D.EnumName.fresh ("eoption", Pos.no_pos)

let none_constr : D.EnumConstructor.t =
  D.EnumConstructor.fresh ("ENone", Pos.no_pos)

let some_constr : D.EnumConstructor.t =
  D.EnumConstructor.fresh ("ESome", Pos.no_pos)

let option_enum_config : (D.EnumConstructor.t * D.typ Marked.pos) list =
  [none_constr, (D.TLit D.TUnit, Pos.no_pos); some_constr, (D.TAny, Pos.no_pos)]

let make_none (pos : Pos.t) : expr Marked.pos Bindlib.box =
  let mark : 'a -> 'a Marked.pos = Marked.mark pos in
  Bindlib.box @@ mark
  @@ EInj
       (mark @@ ELit LUnit, 0, option_enum, [D.TLit D.TUnit, pos; D.TAny, pos])

let make_some (e : expr Marked.pos Bindlib.box) : expr Marked.pos Bindlib.box =
  let pos = Marked.get_mark @@ Bindlib.unbox e in
  let mark : 'a -> 'a Marked.pos = Marked.mark pos in
  begin[@ocamlformat "disable"]
    let+ e = e in
    mark @@ EInj (e, 1, option_enum, [ (D.TLit D.TUnit, pos); (D.TAny, pos) ])
  end

(** [make_matchopt_with_abs_arms arg e_none e_some] build an expression
    [match arg with |None -> e_none | Some -> e_some] and requires e_some and
    e_none to be in the form [EAbs ...].*)
let make_matchopt_with_abs_arms
    (arg : expr Marked.pos Bindlib.box)
    (e_none : expr Marked.pos Bindlib.box)
    (e_some : expr Marked.pos Bindlib.box) : expr Marked.pos Bindlib.box =
  let pos = Marked.get_mark @@ Bindlib.unbox arg in
  let mark : 'a -> 'a Marked.pos = Marked.mark pos in
  begin[@ocamlformat "disable"]
    let+ arg = arg
    and+ e_none = e_none
    and+ e_some = e_some in
    mark @@ EMatch (arg, [ e_none; e_some ], option_enum)
  end

(** [make_matchopt pos v tau arg e_none e_some] builds an expression
    [match arg with | None () -> e_none | Some v -> e_some]. It binds v to
    e_some, permitting it to be used inside the expression. There is no
    requirements on the form of both e_some and e_none. *)
let make_matchopt
    (pos : Pos.t)
    (v : Var.t)
    (tau : D.typ Marked.pos)
    (arg : expr Marked.pos Bindlib.box)
    (e_none : expr Marked.pos Bindlib.box)
    (e_some : expr Marked.pos Bindlib.box) : expr Marked.pos Bindlib.box =
  let x = Var.make ("_", pos) in

  make_matchopt_with_abs_arms arg
    (make_abs (Array.of_list [x]) e_none pos [D.TLit D.TUnit, pos] pos)
    (make_abs (Array.of_list [v]) e_some pos [tau] pos)

let handle_default = Var.make ("handle_default", Pos.no_pos)
let handle_default_opt = Var.make ("handle_default_opt", Pos.no_pos)

type binder = (expr, expr Marked.pos) Bindlib.binder
