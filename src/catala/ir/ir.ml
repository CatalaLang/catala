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

module ApplicationFieldParam = Id.WithId (struct
  type t = string Pos.marked

  let to_string x = Pos.unmark x
end)

module StructFieldName = Id.WithId (struct
  type t = string Pos.marked

  let to_string x = Pos.unmark x
end)

module EnumCase = Id.WithId (struct
  type t = string Pos.marked

  let to_string x = Pos.unmark x
end)

module Field = Id.WithId (struct
  type t = string Pos.marked

  let to_string x = Pos.unmark x
end)

module Enum = Id.WithId (struct
  type t = string Pos.marked

  let to_string x = Pos.unmark x
end)

type qident_element =
  | Ident of ApplicationFieldParam.t
  | StructField of StructFieldName.t
  | EnumConstructor of EnumCase.t
  | ApplicationFieldName of Field.t

type qident = qident_element Pos.marked list

let qident_element_to_string = function
  | Ident x -> ApplicationFieldParam.to_string x
  | StructField x -> StructFieldName.to_string x
  | EnumConstructor x -> EnumCase.to_string x
  | ApplicationFieldName x -> Field.to_string x

module Var = Id.WithId (struct
  type t = qident

  let to_string (qid : qident) =
    String.concat "." (List.map (fun x -> qident_element_to_string (Pos.unmark x)) qid)
end)

(* Type *)

type constructor = string

type primitive_typ = Integer | Decimal | Boolean | Money | Text | Date | Named of constructor

type base_typ_data = {
  typ_data_collection : Pos.t option;
  typ_data_optional : Pos.t option;
  typ_data_base : primitive_typ Pos.marked;
}

type base_typ = Condition | Data of base_typ_data

type func_typ = { arg_typ : base_typ Pos.marked; return_typ : base_typ Pos.marked }

type typ = Base of base_typ | Func of func_typ

(* Expressions *)

type match_case_pattern = constructor Pos.marked list * Var.t option

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
  match_case_expr : expression Pos.marked;
}

and match_cases = match_case Pos.marked list

and expression =
  | MatchWith of expression Pos.marked * match_cases Pos.marked
  | IfThenElse of expression Pos.marked * expression Pos.marked * expression Pos.marked
  | Binop of binop Pos.marked * expression Pos.marked * expression Pos.marked
  | Unop of unop Pos.marked * expression Pos.marked
  | CollectionOp of collection_op Pos.marked * Var.t * expression Pos.marked * expression Pos.marked
  | MemCollection of expression Pos.marked * expression Pos.marked
  | TestMatchCase of expression Pos.marked * constructor Pos.marked
  | FunCall of expression Pos.marked * expression Pos.marked
  | Builtin of builtin_expression
  | Literal of literal
  | Inject of constructor Pos.marked * expression Pos.marked option
  | Project of expression Pos.marked * constructor Pos.marked
  | FuncParameter
  | Var of Var.t

(* a variable shall now be referred by its unique id *)

(* Enum declaration *)
type enum_decl_case = {
  enum_decl_case_name : EnumCase.t;
  enum_decl_case_typ : typ Pos.marked option;
}

type enum_decl = { enum_decl_cases : enum_decl_case Pos.marked list }

(* Fields *)

type field_context_item = {
  field_context_item_name : ApplicationFieldParam.t;
  field_context_item_typ : typ Pos.marked;
}

type field_include_join = {
  parent_field_name : Field.t;
  parent_field_context_item : ApplicationFieldParam.t;
  sub_field_context_item : ApplicationFieldParam.t;
}

type field_include = {
  field_include_sub_field : Field.t;
  field_include_joins : field_include_join Pos.marked list;
}

(* In rule and definition, we keep the parameter for the name, but its uid shall
 * be -1 *)

type rule = {
  rule_parameter : Pos.t option;
  rule_condition : expression Pos.marked option;
  rule_consequence : bool;
}

type definition = {
  definition_parameter : Pos.t option;
  definition_condition : expression Pos.marked option;
  definition_expr : expression Pos.marked;
}

type assertion = {
  assertion_condition : expression Pos.marked option;
  assertion_content : expression Pos.marked;
}

type variation_typ = Increasing | Decreasing

type reference_typ = Decree | Law

type meta_assertion =
  | FixedBy of reference_typ
  | VariesWith of expression Pos.marked * variation_typ Pos.marked option

module UidMap = Map.Make (Int32)

type field = {
  field_var_map : Var.t UidMap.t;
  field_context : field_context_item Pos.marked list;
  field_includes : field_include Pos.marked list;
  field_rules : rule list UidMap.t;
  field_defs : definition list UidMap.t;
  field_assertions : assertion list;
  field_meta_assertions : meta_assertion list UidMap.t;
}

module EnumMap = Map.Make (Enum)
module FieldMap = Map.Make (Field)

type prgm = { enums : enum_decl EnumMap.t; fields : field FieldMap.t }
