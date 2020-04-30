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
type match_case_pattern =
  | PEnum of EnumCase.t Pos.marked * match_case_pattern
  | PVar of Pos.t
  | PWild

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
  | CollectionOp of collection_op Pos.marked * Var.t * expression * expression
  | MemCollection of expression * expression
  | TestMatchCase of expression * EnumCase.t Pos.marked
  | FunCall of expression * expression
  | Builtin of builtin_expression Pos.marked
  | Literal of literal
  | Inject of constructor Pos.marked * expression option
  | Project of expression * constructor Pos.marked
  | BindingParameter of int (* The integer is the De Bruijn index *)
  | Var of Var.t Pos.marked

(* Wrappers *)

type 'a with_type = { value : 'a Pos.marked; typ : typ Pos.marked }

(* Struct declaration *)
type struct_decl_field = StructField.t with_type

type struct_decl = struct_decl_field Pos.marked list

(* Enum declaration *)
type enum_decl_case = EnumCase.t with_type

type enum_decl = enum_decl_case Pos.marked list

(* Fields *)

type field_parameter = ApplicationFieldParam.t with_type

type field_include_join = {
  parent_field_name : Field.t Pos.marked;
  parent_field_context_item : ApplicationFieldParam.t Pos.marked;
  sub_field_context_item : ApplicationFieldParam.t Pos.marked;
}

type field_include = {
  field_include_sub_field : Field.t Pos.marked;
  field_include_joins : field_include_join Pos.marked list;
}

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

type field = {
  field_var_map : qident VarMap.t;
  field_parameters : field_parameter Pos.marked list;
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
