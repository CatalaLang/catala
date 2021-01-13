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

(** Abstract syntax tree built by the Catala parser *)

module Pos = Utils.Pos

type constructor = string
(** Constructors are CamelCase *)

type ident = string
(** Idents are snake_case *)

type qident = ident Pos.marked list

type primitive_typ =
  | Integer
  | Decimal
  | Boolean
  | Money
  | Duration
  | Text
  | Date
  | Named of constructor

type base_typ_data = Primitive of primitive_typ | Collection of base_typ_data Pos.marked

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

type match_case_pattern = constructor Pos.marked list * ident Pos.marked option

type op_kind =
  | KInt  (** No suffix *)
  | KDec  (** Suffix: [.] *)
  | KMoney  (** Suffix: [$] *)
  | KDate  (** Suffix: [@] *)
  | KDuration  (** Suffix: [^] *)

type binop =
  | And
  | Or
  | Add of op_kind
  | Sub of op_kind
  | Mult of op_kind
  | Div of op_kind
  | Lt of op_kind
  | Lte of op_kind
  | Gt of op_kind
  | Gte of op_kind
  | Eq
  | Neq

type unop = Not | Minus of op_kind

type builtin_expression = Cardinal | IntToDec | GetDay | GetMonth | GetYear

type literal_date = {
  literal_date_day : int Pos.marked;
  literal_date_month : int Pos.marked;
  literal_date_year : int Pos.marked;
}

type literal_number = Int of Z.t | Dec of Z.t * Z.t

type literal_unit = Percent | Year | Month | Day

type money_amount = { money_amount_units : Z.t; money_amount_cents : Z.t }

type literal =
  | Number of literal_number Pos.marked * literal_unit Pos.marked option
  | Bool of bool
  | MoneyAmount of money_amount
  | Date of literal_date

type aggregate_func =
  | AggregateSum of primitive_typ
  | AggregateCount
  | AggregateExtremum of bool (* true if max *) * primitive_typ * expression Pos.marked
  | AggregateArgExtremum of bool (* true if max *) * primitive_typ * expression Pos.marked

and collection_op = Exists | Forall | Aggregate of aggregate_func | Map | Filter

and match_case = {
  match_case_pattern : match_case_pattern Pos.marked;
  match_case_expr : expression Pos.marked;
}

and match_cases = match_case Pos.marked list

and expression =
  | MatchWith of expression Pos.marked * match_cases Pos.marked
  | IfThenElse of expression Pos.marked * expression Pos.marked * expression Pos.marked
  | Binop of binop Pos.marked * expression Pos.marked * expression Pos.marked
  | Unop of unop Pos.marked * expression Pos.marked
  | CollectionOp of
      collection_op Pos.marked * ident Pos.marked * expression Pos.marked * expression Pos.marked
  | MemCollection of expression Pos.marked * expression Pos.marked
  | TestMatchCase of expression Pos.marked * constructor Pos.marked
  | FunCall of expression Pos.marked * expression Pos.marked
  | Builtin of builtin_expression
  | Literal of literal
  | EnumInject of constructor Pos.marked * expression Pos.marked option
  | EnumProject of expression Pos.marked * constructor Pos.marked
  | StructLit of constructor Pos.marked * (ident Pos.marked * expression Pos.marked) list
  | ArrayLit of expression Pos.marked list
  | Ident of ident
  | Dotted of expression Pos.marked * ident Pos.marked
      (** Dotted is for both struct field projection and sub-scope variables *)

type rule = {
  rule_label : ident Pos.marked option;
  rule_exception_to : ident Pos.marked option;
  rule_parameter : ident Pos.marked option;
  rule_condition : expression Pos.marked option;
  rule_name : qident Pos.marked;
  rule_consequence : bool Pos.marked;
}

type definition = {
  definition_label : ident Pos.marked option;
  definition_exception_to : ident Pos.marked option;
  definition_name : qident Pos.marked;
  definition_parameter : ident Pos.marked option;
  definition_condition : expression Pos.marked option;
  definition_expr : expression Pos.marked;
}

type variation_typ = Increasing | Decreasing

type meta_assertion =
  | FixedBy of qident Pos.marked * ident Pos.marked
  | VariesWith of qident Pos.marked * expression Pos.marked * variation_typ Pos.marked option

type assertion = {
  assertion_condition : expression Pos.marked option;
  assertion_content : expression Pos.marked;
}

type scope_use_item =
  | Rule of rule
  | Definition of definition
  | Assertion of assertion
  | MetaAssertion of meta_assertion

type scope_use = {
  scope_use_condition : expression Pos.marked option;
  scope_use_name : constructor Pos.marked;
  scope_use_items : scope_use_item Pos.marked list;
}

type scope_decl_context_scope = {
  scope_decl_context_scope_name : ident Pos.marked;
  scope_decl_context_scope_sub_scope : constructor Pos.marked;
}

type scope_decl_context_data = {
  scope_decl_context_item_name : ident Pos.marked;
  scope_decl_context_item_typ : typ Pos.marked;
}

type scope_decl_context_item =
  | ContextData of scope_decl_context_data
  | ContextScope of scope_decl_context_scope

type scope_decl = {
  scope_decl_name : constructor Pos.marked;
  scope_decl_context : scope_decl_context_item Pos.marked list;
}

type code_item =
  | ScopeUse of scope_use
  | ScopeDecl of scope_decl
  | StructDecl of struct_decl
  | EnumDecl of enum_decl

type code_block = code_item Pos.marked list

type source_repr = string Pos.marked

type law_article = {
  law_article_name : string Pos.marked;
  law_article_id : string option;
  law_article_expiration_date : string option;
}

type law_include =
  | PdfFile of string Pos.marked * int option
  | CatalaFile of string Pos.marked
  | LegislativeText of string Pos.marked

type law_article_item = LawText of string | CodeBlock of code_block * source_repr

type law_heading = { law_heading_name : string; law_heading_precedence : int }

type law_structure =
  | LawInclude of law_include
  | LawHeading of law_heading * law_structure list
  | LawArticle of law_article * law_article_item list
  | MetadataBlock of code_block * source_repr
  | IntermediateText of string

type program_item = LawStructure of law_structure

type program = { program_items : program_item list; program_source_files : string list }

type source_file_or_master =
  | SourceFile of program_item list
  | MasterFile of string Pos.marked list
