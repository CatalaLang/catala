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
  | TTuple of typ Pos.marked list
  | TArrow of typ Pos.marked * typ Pos.marked

type lit = LTrue | LFalse | LEmptyError

type expr =
  | EVar of expr Pos.marked Bindlib.var
  | ETuple of expr Pos.marked list
  | ETupleAccess of expr Pos.marked * int
  | ELit of lit
  | EAbs of Pos.t * (expr Pos.marked, expr Pos.marked) Bindlib.binder * typ
  | EApp of expr Pos.marked * expr Pos.marked
  | EDefault of expr Pos.marked * expr Pos.marked * expr Pos.marked list
  | EIfThenElse of expr Pos.marked * expr Pos.marked * expr Pos.marked

module Var = struct
  type t = expr Pos.marked Bindlib.var

  let make (s : string Pos.marked) =
    Bindlib.new_var (fun x -> (EVar x, Pos.get_position s)) (Pos.unmark s)

  let compare x y = Bindlib.compare_vars x y
end

module VarMap = Map.Make (Var)

let make_var (x : Var.t) : expr Pos.marked Bindlib.box = Bindlib.box_var x

let make_abs (x : Var.t) (e : expr Pos.marked Bindlib.box) (pos_binder : Pos.t) (tau : typ)
    (pos : Pos.t) : expr Pos.marked Bindlib.box =
  Bindlib.box_apply (fun b -> (EAbs (pos_binder, b, tau), pos)) (Bindlib.bind_var x e)

let make_app (e : expr Pos.marked Bindlib.box) (u : expr Pos.marked Bindlib.box) (pos : Pos.t) :
    expr Pos.marked Bindlib.box =
  Bindlib.box_apply2 (fun e u -> (EApp (e, u), pos)) e u

type binder = (expr, expr Pos.marked) Bindlib.binder
