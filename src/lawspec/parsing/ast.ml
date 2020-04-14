(* This file is part of the Lawspec compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2019 Inria, contributor: Denis Merigoux
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

type code_item =
  | FieldUse of unit
  | FieldDecl of unit
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
