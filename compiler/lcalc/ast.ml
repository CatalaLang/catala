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
  | ECatch of expr Pos.marked * except * expr Pos.marked (* TODO: temporary *)
  (* semantics of ESome and ENone should be easy to comprehend. *)
  (* The semantics of EMatchopt i've choosen are as following :

     Context :

     C := ... | matchopt [.] e2 e3

     And the rules :

     ---------------------------------- matchopt (Some e1) e2 e3 ~~> e3 e1

     -------------------------- matchopt None e2 e3 ~~> e2 *)
  | EMatchopt of expr Pos.marked * expr Pos.marked * expr Pos.marked
  | ESome of expr Pos.marked
  | ENone

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



let option_enum = D.EnumName.fresh ("eoption", Pos.no_pos)

let make_none (pos: Pos.t) =
  (* Hack: type is not printed in to_ocaml, so I ignore it. *)

  let mark : 'a -> 'a Pos.marked = Pos.mark pos in
  Bindlib.box @@ mark @@ EInj (mark @@ ELit LUnit, 0, option_enum, [])

let make_some (e: expr Pos.marked Bindlib.box): expr Pos.marked Bindlib.box =

  let pos = Pos.get_position @@ Bindlib.unbox e in
  let mark: 'a -> 'a Pos.marked = Pos.mark pos in

  let+ e = e in
  mark @@ EInj (e, 1, option_enum, [])

let make_matchopt
  (e: expr Pos.marked Bindlib.box)
  (e_none: expr Pos.marked Bindlib.box)
  (e_some: expr Pos.marked Bindlib.box): expr Pos.marked Bindlib.box =

  let pos = Pos.get_position @@ Bindlib.unbox e in
  let mark: 'a -> 'a Pos.marked = Pos.mark pos in

  let+ e = e
  and+ e_none = e_none
  and+ e_some = e_some in

  mark @@ EMatch (e, [e_none; e_some], option_enum)



let make_letopt_in
  (x : Var.t)
  (tau : D.typ Pos.marked)
  (e1 : expr Pos.marked Bindlib.box)
  (e2 : expr Pos.marked Bindlib.box) : expr Pos.marked Bindlib.box =
  (* let%opt x: tau = e1 in e2 == matchopt e1 with | None -> None | Some x -> e2 *)
  let pos = Pos.get_position (Bindlib.unbox e2) in

  make_matchopt
    e1
    (make_none pos)
    (make_abs (Array.of_list [ x ]) e2 pos [ tau ] pos)

let handle_default = Var.make ("handle_default", Pos.no_pos)

type binder = (expr, expr Pos.marked) Bindlib.binder

type program = { decl_ctx : D.decl_ctx; scopes : (Var.t * expr Pos.marked) list }
