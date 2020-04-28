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

(* TODO : change this with a WithId ? *)
type qident = string

type primitive_typ = TInteger | TDecimal | TBoolean | TMoney | TText | TDate

type typ_bound = int

type styp =
  | TPrimitive of primitive_typ
  | TBound of typ_bound
  | TFun of styp * styp
  | TSum of styp list

type typ = TTyp of styp | TPoly of typ

type litteral_date = {
  literal_date_day : int Pos.marked;
  literal_date_month : int Pos.marked;
  literal_date_year : int Pos.marked;
}

type const = Int of int | Dec of int * int | Date of litteral_date | Bool of bool

type arith_binop = Add | Sub | Mult | Div | Lt | Lte | Gt | Gte | Eq | Neq

type bool_binop = And | Or

type op = ArithBinop of arith_binop | BoolBinop of bool_binop | Minus | Not

type builtin = Cardinal | AggregateSum | AggregateCount | Now

type binding = qident * typ

type term = untyped_term Pos.marked * typ option

and untyped_term =
  | EConst of const
  | EOp of op
  | EBuiltin of builtin
  | EIfThenElse
  | EExists
  | EForall
  | EVar of binding
  | EFun of binding * term
  | EApp of term * term
  | EInj of qident * term
  | ECase of term * (qident * term) list
