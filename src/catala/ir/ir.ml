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
  type t = string

  let to_string x = x
end)

module StructField = Id.WithId (struct
  type t = string

  let to_string x = x
end)

module EnumCase = Id.WithId (struct
  type t = string

  let to_string x = x
end)

module Field = Id.WithId (struct
  type t = string

  let to_string x = x
end)

module Struct = Id.WithId (struct
  type t = string

  let to_string x = x
end)

module Enum = Id.WithId (struct
  type t = string

  let to_string x = x
end)

type qident = {
  qident_field : Field.t option;
  qident_base : ApplicationFieldParam.t;
  qident_path : StructField.t list;
}

module Var = Id.WithId (struct
  type t = qident

  let to_string (qid : qident) =
    let field = match qid.qident_field with Some x -> Field.raw x | None -> "" in
    let base = ApplicationFieldParam.raw qid.qident_base in
    String.concat "." ([ field; base ] @ List.map StructField.raw qid.qident_path)
end)

module VarMap = Map.Make (Var)

type constructor = Struct of Struct.t | Enum of Enum.t

(* Type *)

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

(* The [bool] argument is true if the match case introduces a pattern *)
type match_case_pattern = constructor Pos.marked list * bool

type binop = And | Or | Add | Sub | Mult | Div | Lt | Lte | Gt | Gte | Eq | Neq

type unop = Not | Minus

type builtin_expression = Cardinal | Now

type aggregate_func = AggregateSum | AggregateCount

type litteral_date = {
  litteral_date_day : int Pos.marked;
  litteral_date_month : int Pos.marked;
  litteral_date_year : int Pos.marked;
}

type litteral_number = Int of int | Dec of int * int

type litteral_unit = Percent | Euro | Year | Month | Day

type collection_op = Exists | Forall | Aggregate of aggregate_func

type litteral =
  | Number of litteral_number Pos.marked * litteral_unit Pos.marked option
  | Date of litteral_date

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
  | Literal of litteral
  | Inject of constructor Pos.marked * expression Pos.marked option
  | Project of expression Pos.marked * constructor Pos.marked
  | BindingParameter of int (* The integer is the De Bruijn index *)
  | Var of Var.t Pos.marked

(* Struct declaration *)
type struct_decl_field = {
  struct_decl_field_name : StructField.t Pos.marked;
  struct_decl_field_typ : typ Pos.marked;
}

type struct_decl = { struct_decl_fields : struct_decl_field Pos.marked list }

(* Enum declaration *)
type enum_decl_case = {
  enum_decl_case_name : EnumCase.t Pos.marked;
  enum_decl_case_typ : typ Pos.marked option;
}

type enum_decl = { enum_decl_cases : enum_decl_case Pos.marked list }

(* Fields *)

type field_context_item = {
  field_context_item_name : ApplicationFieldParam.t Pos.marked;
  field_context_item_typ : typ Pos.marked;
}

type field_include_join = {
  parent_field_name : Field.t Pos.marked;
  parent_field_context_item : ApplicationFieldParam.t Pos.marked;
  sub_field_context_item : ApplicationFieldParam.t Pos.marked;
}

type field_include = {
  field_include_sub_field : Field.t Pos.marked;
  field_include_joins : field_include_join Pos.marked list;
}

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
  | FixedBy of reference_typ Pos.marked
  | VariesWith of expression Pos.marked * variation_typ Pos.marked option

type field = {
  field_var_map : qident VarMap.t;
  field_context : field_context_item Pos.marked list;
  field_includes : field_include Pos.marked list;
  field_rules : rule list VarMap.t;
  field_defs : definition list VarMap.t;
  field_assertions : assertion list;
  field_meta_assertions : meta_assertion list VarMap.t;
}

module EnumMap = Map.Make (Enum)
module FieldMap = Map.Make (Field)
module StructMap = Map.Make (Struct)

type prgm = {
  enums : enum_decl EnumMap.t;
  fields : field FieldMap.t;
  structs : struct_decl StructMap.t;
}
