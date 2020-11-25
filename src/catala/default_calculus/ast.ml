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

type typ =
  | TBool
  | TUnit
  | TInt
  | TTuple of typ Pos.marked list
  | TArrow of typ Pos.marked * typ Pos.marked

type lit = LBool of bool | LEmptyError | LInt of Int64.t

type binop = And | Or | Add | Sub | Mult | Div | Lt | Lte | Gt | Gte | Eq | Neq

type unop = Not | Minus

type operator = Binop of binop | Unop of unop

type expr =
  | EVar of expr Pos.marked Bindlib.var
  | ETuple of expr Pos.marked list
  | ETupleAccess of expr Pos.marked * int
  | ELit of lit
  | EAbs of Pos.t * (expr Pos.marked, expr Pos.marked) Bindlib.mbinder * typ list
  | EApp of expr Pos.marked * expr Pos.marked list
  | EOp of operator
  | EDefault of expr Pos.marked * expr Pos.marked * expr Pos.marked list
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked

module Var = struct
  type t = expr Pos.marked Bindlib.var

  let make (s : string Pos.marked) =
    Bindlib.new_var (fun x -> (EVar x, Pos.get_position s)) (Pos.unmark s)

  let compare x y = Bindlib.compare_vars x y
end

module VarMap = Map.Make (Var)

type vars = expr Pos.marked Bindlib.mvar

let make_var (x : Var.t) : expr Pos.marked Bindlib.box = Bindlib.box_var x

let make_abs (xs : vars) (e : expr Pos.marked Bindlib.box) (pos_binder : Pos.t) (taus : typ list)
    (pos : Pos.t) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply (fun b -> (EAbs (pos_binder, b, taus), pos)) (Bindlib.bind_mvar xs e)

let make_app (e : expr Pos.marked Bindlib.box) (u : expr Pos.marked Bindlib.box list) (pos : Pos.t)
    : expr Pos.marked Bindlib.box =
  Bindlib.box_apply2 (fun e u -> (EApp (e, u), pos)) e (Bindlib.box_list u)

let make_let_in (x : Var.t Pos.marked) (tau : typ) (e1 : expr Pos.marked)
    (e2 : expr Pos.marked Bindlib.box) (pos : Pos.t) : expr Pos.marked =
  ( EApp
      ( Bindlib.unbox (make_abs (Array.of_list [ Pos.unmark x ]) e2 (Pos.get_position x) [ tau ] pos),
        [ e1 ] ),
    pos )

type binder = (expr, expr Pos.marked) Bindlib.binder
