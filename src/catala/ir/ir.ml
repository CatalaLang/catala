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

(* Identifiers *)

type uid = Context.uid

type ident = string

type qident = ident list

module UidMap = Uid.UidMap

(* The [bool] argument is true if the match case introduces a pattern *)
type match_case_pattern = PEnum of uid Pos.marked * match_case_pattern | PVar of Pos.t | PWild

type binop = And | Or | Add | Sub | Mult | Div | Lt | Lte | Gt | Gte | Eq | Neq

type unop = Not | Minus

type builtin_expression = Cardinal | Now

type aggregate_func = AggregateSum | AggregateCount

type literal_date = {
  literal_date_day : int Pos.marked;
  literal_date_month : int Pos.marked;
  literal_date_year : int Pos.marked;
}

type literal_number = Int of int | Dec of int * int

type literal_unit = Percent | Euro | Year | Month | Day

type collection_op = Exists | Forall | Aggregate of aggregate_func

type literal =
  | Number of literal_number Pos.marked * literal_unit Pos.marked option
  | Date of literal_date

type match_case = {
  match_case_pattern : match_case_pattern Pos.marked;
  match_case_expr : expression;
}

and match_cases = match_case Pos.marked list

and expression = expression' Pos.marked

and expression' =
  | MatchWith of expression * match_cases Pos.marked
  | IfThenElse of expression * expression * expression
  | Binop of binop Pos.marked
  | Unop of unop Pos.marked
  | CollectionOp of collection_op Pos.marked * uid * expression * expression
  | MemCollection of expression * expression
  | TestMatchCase of expression * uid
  | FunCall of expression * expression
  | Builtin of builtin_expression Pos.marked
  | Literal of literal
  | Inject of uid Pos.marked * expression option
  | Project of expression * uid Pos.marked
  | BindingParameter of int (* The integer is the De Bruijn index *)
  | Var of uid Pos.marked

(* Scopes *)
type binder = string Pos.marked

type rule = {
  rule_parameter : binder option;
  rule_condition : expression option;
  rule_consequence : bool;
}

type definition = {
  definition_parameter : binder option;
  definition_condition : expression option;
  definition_expr : expression;
}

type assertion = expression

type variation_typ = Increasing | Decreasing

type reference_typ = Decree | Law

type meta_assertion =
  | FixedBy of reference_typ Pos.marked
  | VariesWith of expression * variation_typ Pos.marked option

type scope = {
  scope_use_condition : expression option;
  scope_var_name : qident UidMap.t;
  scope_var_type : Context.typ UidMap.t;
  scope_rules : rule list UidMap.t;
  scope_defs : definition list UidMap.t;
  scope_assertions : assertion list;
  scope_meta_assertions : meta_assertion list UidMap.t;
}

type program_use = scope list UidMap.t

type program = scope UidMap.t
