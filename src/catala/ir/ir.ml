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

type constructor = string

type ident = string

type qident_element = Ident of ident | Constructor of constructor

type qident = qident_element Pos.marked list

module VariableWithId = Id.WithId (struct
  type t = qident

  let to_string qid =
    String.concat "."
      (List.map (fun e -> match Pos.unmark e with Ident x | Constructor x -> x) qid)
end)

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
  | CollectionOp of
      collection_op Pos.marked * ident Pos.marked * expression Pos.marked * expression Pos.marked
  | MemCollection of expression Pos.marked * expression Pos.marked
  | TestMatchCase of expression Pos.marked * constructor Pos.marked
  | FunCall of expression Pos.marked * expression Pos.marked
  | Builtin of builtin_expression
  | Literal of literal
  | Inject of constructor Pos.marked * expression Pos.marked option
  | Project of expression Pos.marked * constructor Pos.marked
  | Var of int

(* a variable shall now be referred by its unique id *)

(* Struct declaration *)
type struct_decl_field = {
  struct_decl_field_name : ident Pos.marked;
  struct_decl_field_typ : typ Pos.marked;
}

type struct_decl = { struct_decl_fields : struct_decl_field Pos.marked list }

(* Enum declaration *)
type enum_decl_case = {
  enum_decl_case_name : constructor Pos.marked;
  enum_decl_case_typ : typ Pos.marked option;
}

type enum_decl = { enum_decl_cases : enum_decl_case Pos.marked list }

(* Fields *)

type field_context_item = {
  field_context_item_name : int Pos.marked;
  field_context_item_typ : typ Pos.marked;
}

type field_include_join = {
  parent_field_name : constructor Pos.marked;
  parent_field_context_item : ident Pos.marked;
  sub_field_name : constructor Pos.marked;
  sub_field_context_item : ident Pos.marked;
}

type field_include = {
  field_include_sub_field : constructor Pos.marked;
  field_include_joins : field_include_join Pos.marked list;
}

(* In rule and definition, we keep the parameter for the name, but its uid shall
 * be -1 *)

type rule = {
  rule_parameter : ident Pos.marked option;
  rule_condition : expression Pos.marked option;
  rule_consequence : bool;
}

type definition = {
  definition_parameter : ident Pos.marked option;
  definition_condition : expression Pos.marked option;
  definition_expr : expression Pos.marked;
}

type assertion = {
  assertion_condition : expression Pos.marked option;
  assertion_content : expression Pos.marked;
}

type variation_typ = Increasing | Decreasing

type meta_assertion =
  | FixedBy of qident Pos.marked * ident Pos.marked
  | VariesWith of qident Pos.marked * expression Pos.marked * variation_typ Pos.marked option

module UidMap = Map.Make (Int32)

type field = {
  field_var_map : qident UidMap.t;
  field_context : field_context_item Pos.marked list;
  field_includes : field_include Pos.marked list;
  field_rules : rule list UidMap.t;
  field_defs : definition list UidMap.t;
  field_assertions : assertion list;
  field_meta_assertions : meta_assertion list UidMap.t;
}

module Struct = Id.WithId (struct
  type t = constructor Pos.marked

  let to_string x = Pos.unmark x
end)

module Enum = Id.WithId (struct
  type t = constructor Pos.marked

  let to_string x = Pos.unmark x
end)

module Field = Id.WithId (struct
  type t = constructor Pos.marked

  (* The position corresponds to the field declaration *)

  let to_string x = Pos.unmark x
end)

module StructMap = Map.Make (Struct)
module EnumMap = Map.Make (Enum)
module FieldMap = Map.Make (Field)

type prgm = {
  structs : struct_decl StructMap.t;
  enums : enum_decl EnumMap.t;
  fields : field FieldMap.t;
}
