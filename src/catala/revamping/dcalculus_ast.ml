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

type typ = TBool | TUnit | TArrow of typ Pos.marked * typ Pos.marked

type lit = LTrue | LFalse | LEmptyError | LConflictError

type expr =
  | EVar of expr Bindlib.var
  | ELit of lit
  | EAbs of Pos.t * (expr, expr Pos.marked) Bindlib.binder * typ
  | EApp of expr Pos.marked * expr Pos.marked
  | EDefault of expr Pos.marked * expr Pos.marked * expr Pos.marked list

module Var = struct
  type t = expr Bindlib.var

  let compare x y = Bindlib.compare_vars x y
end

module VarMap = Map.Make (Var)

type binder = (expr, expr Pos.marked) Bindlib.binder
