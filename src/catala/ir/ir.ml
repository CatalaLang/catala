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

(* Constructor and identifiers *)

type uid = int

module UidMap = Map.Make (Int)

type ident_typ = ScopeParam | StructField | EnumCase | Scope | Struct | Enum

type ident = ident_typ * string

type qident = ident list

type tvar = int

module VarMap = Map.Make (Int)

type constructor = ident

(* Type *)

type primitive_typ =
  | Integer
  | Decimal
  | Boolean
  | Money
  | Text
  | Date
  | Named of constructor
  | Unit

type base_typ_data = TVec of base_typ_data | TOption of base_typ_data | TPrim of primitive_typ

type base_typ = Condition | Data of base_typ_data

type func_typ = { arg_typ : base_typ Pos.marked; return_typ : base_typ Pos.marked }

type typ = Base of base_typ | Func of func_typ

(* Expressions *)

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
  | Inject of constructor Pos.marked * expression option
  | Project of expression * constructor Pos.marked
  | BindingParameter of int (* The integer is the De Bruijn index *)
  | Var of uid Pos.marked

(* Struct declaration *)
type struct_decl = uid Pos.marked list

(* Enum declaration *)
type enum_decl = uid Pos.marked list

(* Scopes *)
(* type scope_context_item = uid

   type scope = { parent_scope_name : uid Pos.marked; parent_scope_context_item : uid Pos.marked;
   sub_scope_context_item : uid Pos.marked; }

   type scope_context_scope = { scope_include_sub_scope : uid Pos.marked;

   }

   type binder = string Pos.marked

   type rule = { rule_parameter : binder option; rule_condition : expression option;
   rule_consequence : bool; }

   type definition = { definition_parameter : binder option; definition_condition : expression
   option; definition_expr : expression; }

   type assertion = expression

   type variation_typ = Increasing | Decreasing

   type reference_typ = Decree | Law

   type meta_assertion = | FixedBy of reference_typ Pos.marked | VariesWith of expression *
   variation_typ Pos.marked option

   type scope = { scope_var_map : qident VarMap.t; scope_context : scope_context_item Pos.marked
   list; scope_includes : scope_include Pos.marked list; scope_rules : rule list VarMap.t;
   scope_defs : definition list VarMap.t; scope_assertions : assertion list; scope_meta_assertions :
   meta_assertion list VarMap.t; }*)

type prgm_item = Struct of struct_decl | Enum of enum_decl

module StringMap = Map.Make (String)

type prgm = {
  items : prgm_item UidMap.t;
  ident_to_uid : uid StringMap.t;
  uid_to_ident : ident UidMap.t;
  types : typ Pos.marked UidMap.t;
}
