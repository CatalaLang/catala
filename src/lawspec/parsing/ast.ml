(* This file is part of the Lawspec compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

type constructor = string

type ident = string

type qident_element = Ident of ident | Constructor of constructor

type qident = qident_element Pos.marked list

type primitive_typ = Integer | Decimal | Boolean | Money | Date | Named of constructor

type base_typ_data = {
  typ_data_collection : Pos.t option;
  typ_data_optional : Pos.t option;
  typ_data_base : primitive_typ Pos.marked;
}

type base_typ = Condition | Data of base_typ_data

type func_typ = { arg_typ : base_typ Pos.marked; return_typ : base_typ Pos.marked }

type typ = Base of base_typ | Func of func_typ

type struct_decl_field = {
  struct_decl_field_name : ident Pos.marked;
  struct_decl_field_typ : typ Pos.marked;
}

type struct_decl = {
  struct_decl_name : constructor Pos.marked;
  struct_decl_fields : struct_decl_field Pos.marked list;
}

type enum_decl_case = {
  enum_decl_case_name : constructor Pos.marked;
  enum_decl_case_typ : typ Pos.marked option;
}

type enum_decl = {
  enum_decl_name : constructor Pos.marked;
  enum_decl_cases : enum_decl_case Pos.marked list;
}

type field_decl_context_item = {
  field_decl_context_item_name : ident Pos.marked;
  field_decl_context_item_typ : typ Pos.marked;
}

type field_decl_include_join = {
  parent_field_name : constructor Pos.marked;
  parent_field_context_item : ident Pos.marked;
  sub_field_name : constructor Pos.marked;
  sub_field_context_item : ident Pos.marked;
}

type field_decl_include = {
  field_decl_include_sub_field : constructor Pos.marked;
  field_decl_include_joins : field_decl_include_join Pos.marked list;
}

type field_decl = {
  field_decl_name : constructor Pos.marked;
  field_decl_context : field_decl_context_item Pos.marked list;
  field_decl_includes : field_decl_include Pos.marked list;
}

type match_case_pattern = constructor Pos.marked list * ident Pos.marked option

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

type literal =
  | Number of literal_number Pos.marked * literal_unit Pos.marked option
  | Date of literal_date

type match_case = {
  match_case_pattern : match_case_pattern Pos.marked;
  match_case_expr : expression Pos.marked;
}

and match_cases = match_case Pos.marked list

and expression =
  | Exists of ident Pos.marked * expression Pos.marked * expression Pos.marked
  | Forall of ident Pos.marked * expression Pos.marked * expression Pos.marked
  | MatchWith of expression Pos.marked * match_cases Pos.marked
  | IfThenElse of expression Pos.marked * expression Pos.marked * expression Pos.marked
  | Binop of binop Pos.marked * expression Pos.marked * expression Pos.marked
  | Unop of unop Pos.marked * expression Pos.marked
  | MemCollection of expression Pos.marked * expression Pos.marked
  | TestMatchCase of expression Pos.marked * constructor Pos.marked
  | FunCall of expression Pos.marked * expression Pos.marked
  | Builtin of builtin_expression
  | Aggregate of
      aggregate_func Pos.marked * ident Pos.marked * expression Pos.marked * expression Pos.marked
  | Literal of literal
  | Inject of constructor Pos.marked * expression Pos.marked option
  | Project of expression Pos.marked * constructor Pos.marked
  | Qident of qident

type rule = {
  rule_parameter : ident Pos.marked option;
  rule_condition : expression Pos.marked option;
  rule_name : qident Pos.marked;
}

type definition = {
  definition_name : qident Pos.marked;
  definition_parameter : ident Pos.marked option;
  definition_condition : expression Pos.marked option;
  definition_expr : expression Pos.marked;
}

type variation_typ = Increasing | Decreasing

type assertion_content =
  | Assert of expression
  | FixedBy of qident Pos.marked * ident Pos.marked
  | VariesWith of qident Pos.marked * expression Pos.marked * variation_typ Pos.marked option

type assertion = {
  assertion_condition : expression Pos.marked option;
  assertion_content : assertion_content Pos.marked;
}

type field_use_item = Rule of rule | Definition of definition | Assertion of assertion

type field_use = {
  field_use_name : constructor Pos.marked;
  field_use_items : field_use_item Pos.marked list;
}

type code_item =
  | FieldUse of field_use
  | FieldDecl of field_decl
  | StructDecl of struct_decl
  | EnumDecl of enum_decl

type code_block = code_item Pos.marked list

type source_repr = string Pos.marked

type source_file_item =
  | LawCode of string
  | LawArticle of string
  | LawText of string
  | CodeBlock of code_block * source_repr
  | MetadataBlock of code_block * source_repr

type source_file = source_file_item list
