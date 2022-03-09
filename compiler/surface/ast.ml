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

[@@@ocaml.warning "-7"]

open Utils
(** {1 Visitor classes for programs} *)

(** To allow for quick traversal and/or modification of this AST structure, we
    provide a {{:https://en.wikipedia.org/wiki/Visitor_pattern} visitor design
    pattern}. This feature is implemented via
    {{:https://gitlab.inria.fr/fpottier/visitors} François Pottier's OCaml
    visitors library}. *)

(** {1 Type definitions} *)

type constructor = (string[@opaque])
[@@deriving
  visitors { variety = "map"; name = "constructor_map"; nude = true },
    visitors { variety = "iter"; name = "constructor_iter"; nude = true }]
(** Constructors are CamelCase *)

type ident = (string[@opaque])
[@@deriving
  visitors { variety = "map"; name = "ident_map"; nude = true },
    visitors { variety = "iter"; name = "ident_iter"; nude = true }]
(** Idents are snake_case *)

type qident = ident Pos.marked list
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "Pos.marked_map"; "ident_map" ];
      name = "qident_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "Pos.marked_iter"; "ident_iter" ];
        name = "qident_iter";
      }]

type primitive_typ =
  | Integer
  | Decimal
  | Boolean
  | Money
  | Duration
  | Text
  | Date
  | Named of constructor
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "constructor_map" ];
      name = "primitive_typ_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "constructor_iter" ];
        name = "primitive_typ_iter";
      }]

type base_typ_data =
  | Primitive of primitive_typ
  | Collection of base_typ_data Pos.marked
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "Pos.marked_map"; "primitive_typ_map" ];
      name = "base_typ_data_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "Pos.marked_iter"; "primitive_typ_iter" ];
        name = "base_typ_data_iter";
      }]

type base_typ = Condition | Data of base_typ_data
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "base_typ_data_map" ];
      name = "base_typ_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "base_typ_data_iter" ];
        name = "base_typ_iter";
        nude = true;
      }]

type func_typ = {
  arg_typ : base_typ Pos.marked;
  return_typ : base_typ Pos.marked;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "base_typ_map" ];
      name = "func_typ_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "base_typ_iter" ];
        name = "func_typ_iter";
        nude = true;
      }]

type typ = Base of base_typ | Func of func_typ
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "func_typ_map" ];
      name = "typ_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "func_typ_iter" ];
        name = "typ_iter";
        nude = true;
      }]

type struct_decl_field = {
  struct_decl_field_name : ident Pos.marked;
  struct_decl_field_typ : typ Pos.marked;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "typ_map"; "ident_map" ];
      name = "struct_decl_field_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "typ_iter"; "ident_iter" ];
        name = "struct_decl_field_iter";
      }]

type struct_decl = {
  struct_decl_name : constructor Pos.marked;
  struct_decl_fields : struct_decl_field Pos.marked list;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "struct_decl_field_map" ];
      name = "struct_decl_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "struct_decl_field_iter" ];
        name = "struct_decl_iter";
      }]

type enum_decl_case = {
  enum_decl_case_name : constructor Pos.marked;
  enum_decl_case_typ : typ Pos.marked option;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "typ_map" ];
      name = "enum_decl_case_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "typ_iter" ];
        name = "enum_decl_case_iter";
        nude = true;
      }]

type enum_decl = {
  enum_decl_name : constructor Pos.marked;
  enum_decl_cases : enum_decl_case Pos.marked list;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "enum_decl_case_map" ];
      name = "enum_decl_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "enum_decl_case_iter" ];
        name = "enum_decl_iter";
        nude = true;
      }]

type match_case_pattern =
  (constructor Pos.marked option * constructor Pos.marked) list
  * ident Pos.marked option
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "ident_map"; "constructor_map"; "Pos.marked_map" ];
      name = "match_case_pattern_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "ident_iter"; "constructor_iter"; "Pos.marked_iter" ];
        name = "match_case_pattern_iter";
      }]

type op_kind = KInt | KDec | KMoney | KDate | KDuration
[@@deriving
  visitors { variety = "map"; name = "op_kind_map"; nude = true },
    visitors { variety = "iter"; name = "op_kind_iter"; nude = true }]

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
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "op_kind_map" ];
      name = "binop_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "op_kind_iter" ];
        name = "binop_iter";
        nude = true;
      }]

type unop = Not | Minus of op_kind
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "op_kind_map" ];
      name = "unop_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "op_kind_iter" ];
        name = "unop_iter";
        nude = true;
      }]

type builtin_expression = Cardinal | IntToDec | GetDay | GetMonth | GetYear
[@@deriving
  visitors { variety = "map"; name = "builtin_expression_map"; nude = true },
    visitors { variety = "iter"; name = "builtin_expression_iter"; nude = true }]

type literal_date = {
  literal_date_day : (int[@opaque]) Pos.marked;
  literal_date_month : (int[@opaque]) Pos.marked;
  literal_date_year : (int[@opaque]) Pos.marked;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "Pos.marked_map" ];
      name = "literal_date_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "Pos.marked_iter" ];
        name = "literal_date_iter";
      }]

type literal_number =
  | Int of (Runtime.integer[@opaque])
  | Dec of (Runtime.integer[@opaque]) * (Runtime.integer[@opaque])
[@@deriving
  visitors { variety = "map"; name = "literal_number_map"; nude = true },
    visitors { variety = "iter"; name = "literal_number_iter"; nude = true }]

type literal_unit = Percent | Year | Month | Day
[@@deriving
  visitors { variety = "map"; name = "literal_unit_map"; nude = true },
    visitors { variety = "iter"; name = "literal_unit_iter"; nude = true }]

type money_amount = {
  money_amount_units : (Runtime.integer[@opaque]);
  money_amount_cents : (Runtime.integer[@opaque]);
}
[@@deriving
  visitors { variety = "map"; name = "money_amount_map"; nude = true },
    visitors { variety = "iter"; name = "money_amount_iter"; nude = true }]

type literal =
  | LNumber of literal_number Pos.marked * literal_unit Pos.marked option
  | LBool of bool
  | LMoneyAmount of money_amount
  | LDate of literal_date
[@@deriving
  visitors
    {
      variety = "map";
      ancestors =
        [
          "literal_number_map";
          "money_amount_map";
          "literal_date_map";
          "literal_unit_map";
        ];
      name = "literal_map";
    },
    visitors
      {
        variety = "iter";
        ancestors =
          [
            "literal_number_iter";
            "money_amount_iter";
            "literal_date_iter";
            "literal_unit_iter";
          ];
        name = "literal_iter";
      }]

type aggregate_func =
  | AggregateSum of primitive_typ
  | AggregateCount
  | AggregateExtremum of bool * primitive_typ * expression Pos.marked
  | AggregateArgExtremum of bool * primitive_typ * expression Pos.marked

and collection_op =
  | Exists
  | Forall
  | Aggregate of aggregate_func
  | Map
  | Filter

and explicit_match_case = {
  match_case_pattern : match_case_pattern Pos.marked;
  match_case_expr : expression Pos.marked;
}

and match_case =
  | WildCard of expression Pos.marked
  | MatchCase of explicit_match_case

and match_cases = match_case Pos.marked list

and expression =
  | MatchWith of expression Pos.marked * match_cases Pos.marked
  | IfThenElse of
      expression Pos.marked * expression Pos.marked * expression Pos.marked
  | Binop of binop Pos.marked * expression Pos.marked * expression Pos.marked
  | Unop of unop Pos.marked * expression Pos.marked
  | CollectionOp of
      collection_op Pos.marked
      * ident Pos.marked
      * expression Pos.marked
      * expression Pos.marked
  | MemCollection of expression Pos.marked * expression Pos.marked
  | TestMatchCase of expression Pos.marked * match_case_pattern Pos.marked
  | FunCall of expression Pos.marked * expression Pos.marked
  | Builtin of builtin_expression
  | Literal of literal
  | EnumInject of
      constructor Pos.marked option
      * constructor Pos.marked
      * expression Pos.marked option
  | StructLit of
      constructor Pos.marked * (ident Pos.marked * expression Pos.marked) list
  | ArrayLit of expression Pos.marked list
  | Ident of ident
  | Dotted of
      expression Pos.marked * constructor Pos.marked option * ident Pos.marked
      (** Dotted is for both struct field projection and sub-scope variables *)
[@@deriving
  visitors
    {
      variety = "map";
      ancestors =
        [
          "primitive_typ_map";
          "match_case_pattern_map";
          "literal_map";
          "binop_map";
          "unop_map";
          "builtin_expression_map";
        ];
      name = "expression_map";
    },
    visitors
      {
        variety = "iter";
        ancestors =
          [
            "primitive_typ_iter";
            "match_case_pattern_iter";
            "literal_iter";
            "binop_iter";
            "unop_iter";
            "builtin_expression_iter";
          ];
        name = "expression_iter";
      }]

type exception_to =
  | NotAnException
  | UnlabeledException
  | ExceptionToLabel of ident Pos.marked
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "ident_map"; "Pos.marked_map" ];
      name = "exception_to_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "ident_iter"; "Pos.marked_iter" ];
        name = "exception_to_iter";
      }]

type rule = {
  rule_label : ident Pos.marked option;
  rule_exception_to : exception_to;
  rule_parameter : ident Pos.marked option;
  rule_condition : expression Pos.marked option;
  rule_name : qident Pos.marked;
  rule_id : Desugared.Ast.RuleName.t; [@opaque]
  rule_consequence : (bool[@opaque]) Pos.marked;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "expression_map"; "qident_map"; "exception_to_map" ];
      name = "rule_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "expression_iter"; "qident_iter"; "exception_to_iter" ];
        name = "rule_iter";
      }]

type definition = {
  definition_label : ident Pos.marked option;
  definition_exception_to : exception_to;
  definition_name : qident Pos.marked;
  definition_parameter : ident Pos.marked option;
  definition_condition : expression Pos.marked option;
  definition_id : Desugared.Ast.RuleName.t; [@opaque]
  definition_expr : expression Pos.marked;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "expression_map"; "qident_map"; "exception_to_map" ];
      name = "definition_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "expression_iter"; "qident_iter"; "exception_to_iter" ];
        name = "definition_iter";
      }]

type variation_typ = Increasing | Decreasing
[@@deriving
  visitors { variety = "map"; name = "variation_typ_map" },
    visitors { variety = "iter"; name = "variation_typ_iter" }]

type meta_assertion =
  | FixedBy of qident Pos.marked * ident Pos.marked
  | VariesWith of
      qident Pos.marked
      * expression Pos.marked
      * variation_typ Pos.marked option
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "variation_typ_map"; "qident_map"; "expression_map" ];
      name = "meta_assertion_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "variation_typ_iter"; "qident_iter"; "expression_iter" ];
        name = "meta_assertion_iter";
      }]

type assertion = {
  assertion_condition : expression Pos.marked option;
  assertion_content : expression Pos.marked;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "expression_map" ];
      name = "assertion_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "expression_iter" ];
        name = "assertion_iter";
      }]

type scope_use_item =
  | Rule of rule
  | Definition of definition
  | Assertion of assertion
  | MetaAssertion of meta_assertion
[@@deriving
  visitors
    {
      variety = "map";
      ancestors =
        [ "meta_assertion_map"; "definition_map"; "assertion_map"; "rule_map" ];
      name = "scope_use_item_map";
    },
    visitors
      {
        variety = "iter";
        ancestors =
          [
            "meta_assertion_iter";
            "definition_iter";
            "assertion_iter";
            "rule_iter";
          ];
        name = "scope_use_item_iter";
      }]

type scope_use = {
  scope_use_condition : expression Pos.marked option;
  scope_use_name : constructor Pos.marked;
  scope_use_items : scope_use_item Pos.marked list;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "expression_map"; "scope_use_item_map" ];
      name = "scope_use_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "expression_iter"; "scope_use_item_iter" ];
        name = "scope_use_iter";
      }]

type io_input = Input | Context | Internal
[@@deriving
  visitors { variety = "map"; name = "io_input_map" },
    visitors { variety = "iter"; name = "io_input_iter" }]

type scope_decl_context_io = {
  scope_decl_context_io_input : io_input Pos.marked;
  scope_decl_context_io_output : bool Pos.marked;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "io_input_map"; "Pos.marked_map" ];
      name = "scope_decl_context_io_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "io_input_iter"; "Pos.marked_iter" ];
        name = "scope_decl_context_io_iter";
      }]

type scope_decl_context_scope = {
  scope_decl_context_scope_name : ident Pos.marked;
  scope_decl_context_scope_sub_scope : constructor Pos.marked;
  scope_decl_context_scope_attribute : scope_decl_context_io;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors =
        [
          "ident_map";
          "constructor_map";
          "scope_decl_context_io_map";
          "Pos.marked_map";
        ];
      name = "scope_decl_context_scope_map";
    },
    visitors
      {
        variety = "iter";
        ancestors =
          [
            "ident_iter";
            "constructor_iter";
            "scope_decl_context_io_iter";
            "Pos.marked_iter";
          ];
        name = "scope_decl_context_scope_iter";
      }]

type scope_decl_context_data = {
  scope_decl_context_item_name : ident Pos.marked;
  scope_decl_context_item_typ : typ Pos.marked;
  scope_decl_context_item_attribute : scope_decl_context_io;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "typ_map"; "scope_decl_context_io_map"; "ident_map" ];
      name = "scope_decl_context_data_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "typ_iter"; "scope_decl_context_io_iter"; "ident_iter" ];
        name = "scope_decl_context_data_iter";
      }]

type scope_decl_context_item =
  | ContextData of scope_decl_context_data
  | ContextScope of scope_decl_context_scope
[@@deriving
  visitors
    {
      variety = "map";
      ancestors =
        [ "scope_decl_context_data_map"; "scope_decl_context_scope_map" ];
      name = "scope_decl_context_item_map";
    },
    visitors
      {
        variety = "iter";
        ancestors =
          [ "scope_decl_context_data_iter"; "scope_decl_context_scope_iter" ];
        name = "scope_decl_context_item_iter";
      }]

type scope_decl = {
  scope_decl_name : constructor Pos.marked;
  scope_decl_context : scope_decl_context_item Pos.marked list;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "scope_decl_context_item_map" ];
      name = "scope_decl_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "scope_decl_context_item_iter" ];
        name = "scope_decl_iter";
      }]

type code_item =
  | ScopeUse of scope_use
  | ScopeDecl of scope_decl
  | StructDecl of struct_decl
  | EnumDecl of enum_decl
[@@deriving
  visitors
    {
      variety = "map";
      ancestors =
        [
          "scope_decl_map"; "enum_decl_map"; "struct_decl_map"; "scope_use_map";
        ];
      name = "code_item_map";
    },
    visitors
      {
        variety = "iter";
        ancestors =
          [
            "scope_decl_iter";
            "enum_decl_iter";
            "struct_decl_iter";
            "scope_use_iter";
          ];
        name = "code_item_iter";
      }]

type code_block = code_item Pos.marked list
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "code_item_map" ];
      name = "code_block_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "code_item_iter" ];
        name = "code_block_iter";
      }]

type source_repr = (string[@opaque]) Pos.marked
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "Pos.marked_map" ];
      name = "source_repr_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "Pos.marked_iter" ];
        name = "source_repr_iter";
      }]

type law_heading = {
  law_heading_name : (string[@opaque]) Pos.marked;
  law_heading_id : (string[@opaque]) option;
  law_heading_expiration_date : (string[@opaque]) option;
  law_heading_precedence : (int[@opaque]);
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "Pos.marked_map" ];
      name = "law_heading_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "Pos.marked_iter" ];
        name = "law_heading_iter";
      }]

type law_include =
  | PdfFile of (string[@opaque]) Pos.marked * (int[@opaque]) option
  | CatalaFile of (string[@opaque]) Pos.marked
  | LegislativeText of (string[@opaque]) Pos.marked
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "Pos.marked_map" ];
      name = "law_include_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "Pos.marked_iter" ];
        name = "law_include_iter";
      }]

type law_structure =
  | LawInclude of law_include
  | LawHeading of law_heading * law_structure list
  | LawText of (string[@opaque])
  | CodeBlock of code_block * source_repr * bool (* Metadata if true *)
[@@deriving
  visitors
    {
      variety = "map";
      ancestors =
        [
          "law_include_map";
          "code_block_map";
          "source_repr_map";
          "law_heading_map";
        ];
      name = "law_structure_map";
    },
    visitors
      {
        variety = "iter";
        ancestors =
          [
            "law_include_iter";
            "code_block_iter";
            "source_repr_iter";
            "law_heading_iter";
          ];
        name = "law_structure_iter";
      }]

type program = {
  program_items : law_structure list;
  program_source_files : (string[@opaque]) list;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = [ "law_structure_map" ];
      name = "program_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = [ "law_structure_iter" ];
        name = "program_iter";
      }]

type source_file = law_structure list

(** {1 Helpers}*)

(** Translates a {!type: rule} into the corresponding {!type: definition} *)
let rule_to_def (rule : rule) : definition =
  let consequence_expr = Literal (LBool (Pos.unmark rule.rule_consequence)) in
  {
    definition_label = rule.rule_label;
    definition_exception_to = rule.rule_exception_to;
    definition_name = rule.rule_name;
    definition_parameter = rule.rule_parameter;
    definition_condition = rule.rule_condition;
    definition_id = rule.rule_id;
    definition_expr = (consequence_expr, Pos.get_position rule.rule_consequence);
  }
