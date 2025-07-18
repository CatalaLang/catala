(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Abstract syntax tree built by the Catala parser *)

open Catala_utils

(** {1 Type definitions} *)

type uident = string
(** Constructors are CamelCase *)

type lident = string
(** Idents are snake_case *)

type path = uident Mark.pos list

type scope_var = lident Mark.pos list
(** [foo.bar] in binding position: used to specify variables of subscopes *)

type primitive_typ =
  | Integer
  | Decimal
  | Boolean
  | Money
  | Duration
  | Text
  | Date
  | Named of path * uident Mark.pos
  | Var of lident Mark.pos option

type base_typ_data =
  | Primitive of primitive_typ
  | Collection of base_typ_data Mark.pos
  | TTuple of base_typ_data Mark.pos list

type base_typ = Condition | Data of base_typ_data

type func_typ = {
  arg_typ : (lident Mark.pos * base_typ Mark.pos) list;
  return_typ : base_typ Mark.pos;
}

type typ = naked_typ Mark.pos
and naked_typ = Base of base_typ | Func of func_typ

type struct_decl_field = {
  struct_decl_field_name : lident Mark.pos;
  struct_decl_field_typ : typ;
}

type struct_decl = {
  struct_decl_name : uident Mark.pos;
  struct_decl_fields : struct_decl_field Mark.pos list;
}

type enum_decl_case = {
  enum_decl_case_name : uident Mark.pos;
  enum_decl_case_typ : typ option;
}

type enum_decl = {
  enum_decl_name : uident Mark.pos;
  enum_decl_cases : enum_decl_case Mark.pos list;
}

type match_case_pattern =
  (path * uident Mark.pos) Mark.pos list * lident Mark.pos option

type op_kind = KPoly | KInt | KDec | KMoney | KDate | KDuration

type binop =
  | And
  | Or
  | Xor
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
  | Concat

type unop = Not | Minus of op_kind

type builtin_expression =
  | Impossible
  | Cardinal
  | ToInteger
  | ToDecimal
  | ToMoney
  | GetDay
  | GetMonth
  | GetYear
  | LastDayOfMonth
  | FirstDayOfMonth
  | Round

type literal_date = {
  literal_date_day : int;
  literal_date_month : int;
  literal_date_year : int;
}

type literal_number = Int of string | Dec of string * string
type literal_unit = Percent | Year | Month | Day
type money_amount = { money_amount_units : string; money_amount_cents : string }

type literal =
  | LNumber of literal_number Mark.pos * literal_unit Mark.pos option
  | LBool of bool
  | LMoneyAmount of money_amount
  | LDate of literal_date

type collection_op =
  | Member of { element : expression }
  | Exists of { predicate : lident Mark.pos list * expression }
  | Forall of { predicate : lident Mark.pos list * expression }
  | Map of { f : lident Mark.pos list * expression }
  | Filter of { f : lident Mark.pos list * expression }
  | AggregateSum of { typ : primitive_typ }
  (* it would be nice to remove the need for specifying the and here like for
     extremums, but we need an additionl overload for "neutral element for
     addition across types" *)
  | AggregateExtremum of { max : bool; default : expression option }
  | AggregateArgExtremum of {
      max : bool;
      default : expression option;
      f : lident Mark.pos list * expression;
    }
  | Fold of {
      f : lident Mark.pos list * lident Mark.pos list * expression;
      init : expression;
    }

and explicit_match_case = {
  match_case_pattern : match_case_pattern Mark.pos;
  match_case_expr : expression;
}

and match_case = WildCard of expression | MatchCase of explicit_match_case
and match_cases = match_case Mark.pos list
and expression = naked_expression Mark.pos

and naked_expression =
  | Paren of expression
  | MatchWith of expression * match_cases Mark.pos
  | IfThenElse of expression * expression * expression
  | Binop of binop Mark.pos * expression * expression
  | Unop of unop Mark.pos * expression
  | CollectionOp of collection_op Mark.pos * expression
  | TestMatchCase of expression * match_case_pattern Mark.pos
  | FunCall of expression * expression list
  | ScopeCall of
      (path * uident Mark.pos) Mark.pos * (lident Mark.pos * expression) list
  | LetIn of lident Mark.pos list * expression * expression
  | Builtin of builtin_expression
  | Literal of literal
  | EnumInject of (path * uident Mark.pos) Mark.pos * expression option
  | StructLit of
      (path * uident Mark.pos) Mark.pos * (lident Mark.pos * expression) list
  | StructReplace of expression * (lident Mark.pos * expression) list
  | ArrayLit of expression list
  | Tuple of expression list
  | Ident of path * lident Mark.pos * lident Mark.pos option
  (* path, ident, state *)
  | Dotted of expression * (path * lident Mark.pos) Mark.pos
      (** Dotted is for both struct field projection and sub-scope variables *)
  | TupleAccess of expression * int Mark.pos

type exception_to =
  | NotAnException
  | UnlabeledException
  | ExceptionToLabel of lident Mark.pos

type rule = {
  rule_label : lident Mark.pos option;
  rule_exception_to : exception_to;
  rule_parameter : lident Mark.pos list Mark.pos option;
  rule_condition : expression option;
  rule_name : scope_var Mark.pos;
  rule_id : Shared_ast.RuleName.t;
  rule_consequence : bool Mark.pos;
  rule_state : lident Mark.pos option;
}

type definition = {
  definition_label : lident Mark.pos option;
  definition_exception_to : exception_to;
  definition_name : scope_var Mark.pos;
  definition_parameter : lident Mark.pos list Mark.pos option;
  definition_condition : expression option;
  definition_id : Shared_ast.RuleName.t;
  definition_expr : expression;
  definition_state : lident Mark.pos option;
}

type variation_typ = Increasing | Decreasing

type assertion = {
  assertion_condition : expression option;
  assertion_content : expression;
}

type scope_use_item =
  | Rule of rule
  | Definition of definition
  | Assertion of assertion
  | DateRounding of variation_typ Mark.pos

type scope_use = {
  scope_use_condition : expression option;
  scope_use_name : uident Mark.pos;
  scope_use_items : scope_use_item Mark.pos list;
}

type io_input = Input | Context | Internal

type scope_decl_context_io = {
  scope_decl_context_io_input : io_input Mark.pos;
  scope_decl_context_io_output : bool Mark.pos;
}

type scope_decl_context_scope = {
  scope_decl_context_scope_name : lident Mark.pos;
  scope_decl_context_scope_sub_scope : (path * uident Mark.pos) Mark.pos;
  scope_decl_context_scope_attribute : scope_decl_context_io;
}

type scope_decl_context_data = {
  scope_decl_context_item_name : lident Mark.pos;
  scope_decl_context_item_typ : typ;
  scope_decl_context_item_parameters :
    (lident Mark.pos * typ) list Mark.pos option;
  scope_decl_context_item_attribute : scope_decl_context_io;
  scope_decl_context_item_states : lident Mark.pos list;
}

type scope_decl_context_item =
  | ContextData of scope_decl_context_data
  | ContextScope of scope_decl_context_scope

type scope_decl = {
  scope_decl_name : uident Mark.pos;
  scope_decl_context : scope_decl_context_item Mark.pos list;
}

type top_def = {
  topdef_name : lident Mark.pos;
  topdef_args : (lident Mark.pos * base_typ Mark.pos) list Mark.pos option;
      (** Empty list if this is not a function *)
  topdef_type : typ;
  topdef_expr : expression option;
}

type code_item =
  | ScopeUse of scope_use
  | ScopeDecl of scope_decl
  | StructDecl of struct_decl
  | EnumDecl of enum_decl
  | Topdef of top_def

type code_block = code_item Mark.pos list
type source_repr = string Mark.pos

type law_heading = {
  law_heading_name : string Mark.pos;
  law_heading_id : string option;
  law_heading_is_archive : bool;
  law_heading_precedence : int;
}

type law_include =
  | PdfFile of string Mark.pos * int option
  | CatalaFile of string Mark.pos
  | LegislativeText of string Mark.pos

type law_structure =
  | LawInclude of law_include
  | ModuleDef of uident Mark.pos * bool (* External if true *)
  | ModuleUse of uident Mark.pos * uident Mark.pos option
  | LawHeading of law_heading * law_structure list
  | LawText of string
  | CodeBlock of code_block * source_repr * bool (* Metadata if true *)

type module_use = {
  mod_use_name : uident Mark.pos;
  mod_use_alias : uident Mark.pos;
}

type program_module = { module_name : uident Mark.pos; module_external : bool }

type program = {
  program_module : program_module option;
  program_items : law_structure list;
  program_source_files : string list;
  program_used_modules : module_use list;
  program_lang : Global.backend_lang;
}

type source_file = law_structure list
type Shared_ast.attr_value += Expression of expression

type module_items =
  | Code of law_structure list
      (** Used in whole-program to gather all module code *)
  | Interface of (code_item Mark.pos * Shared_ast.visibility) list
      (** Invariant: an interface shall only contain [*Decl] elements, or
          [Topdef] elements with [topdef_expr = None] (metadata only). The
          visibility is determined from the presence of the item in a [metadata]
          block, but might later be promoted due to test annotations or
          dependencies *)

type module_content = {
  module_modname : program_module;
  module_items : module_items;
  module_submodules : module_use list;
}
