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

type primitive_typ = TInteger | TDecimal | TBoolean | TMoney | TText | TDate | Unit

type typ_bound = int

type styp =
  | TPrimitive of primitive_typ
  | TBound of typ_bound
  | TArrow of styp * styp
  | TSum of Ir.Enum.t
  | TVec of styp

type typ = TTyp of styp | TPoly of typ

type const = Ir.literal

type arith_binop = Add | Sub | Mult | Div | Lt | Lte | Gt | Gte | Eq | Neq

type bool_binop = And | Or

type op = ArithBinop of arith_binop | BoolBinop of bool_binop | Minus | Not

type builtin = Cardinal | Map | Fold | Now

type binding = Ir.Var.t * typ

type enum_case = Ir.EnumCase.t

type term = untyped_term Pos.marked * typ

and untyped_term =
  | EConst of const
  | EOp of op
  | EBuiltin of builtin
  | EIfThenElse of term * term * term
  | EExists of binding * term * term
  | EForall of binding * term * term
  | EVar of int
  | EFun of binding list * term
  | EApp of term * term list
  | EInj of enum_case * term
  | ECase of term * (enum_case * term) list
  | EPolyIntro of term
  | EPolyApp of term * typ

(* Wrappers *)

type 'expr field = {
  field_parameters : Ir.scope_context_item Pos.marked list;
  field_rules : 'expr Ir.VarMap.t;
  field_defs : 'expr Ir.VarMap.t;
  field_assertion : term list;
}

type 'expr program = {
  enums : Ir.enum_decl Ir.EnumMap.t;
  structs : Ir.struct_decl Ir.StructMap.t;
  fields : 'expr field Ir.ScopeMap.t;
}

type program_with_default_logic = term list program

type program_with_normal_logic = term program
