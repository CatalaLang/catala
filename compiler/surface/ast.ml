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

open Catala_utils
(** {1 Visitor classes for programs} *)

(** To allow for quick traversal and/or modification of this AST structure, we
    provide a {{:https://en.wikipedia.org/wiki/Visitor_pattern} visitor design
    pattern}. This feature is implemented via
    {{:https://gitlab.inria.fr/fpottier/visitors} François Pottier's OCaml
    visitors library}. *)

(** {1 Type definitions} *)

type uident = (string[@opaque])
[@@deriving
  visitors { variety = "map"; name = "uident_map"; nude = true },
    visitors { variety = "iter"; name = "uident_iter"; nude = true }]
(** Constructors are CamelCase *)

type lident = (string[@opaque])
[@@deriving
  visitors { variety = "map"; name = "lident_map"; nude = true },
    visitors { variety = "iter"; name = "lident_iter"; nude = true }]
(** Idents are snake_case *)

type path = uident Mark.pos list
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["Mark.pos_map"; "uident_map"];
      name = "path_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["Mark.pos_iter"; "uident_iter"];
        name = "path_iter";
      }]

type scope_var = lident Mark.pos list
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["Mark.pos_map"; "lident_map"];
      name = "scope_var_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["Mark.pos_iter"; "lident_iter"];
        name = "scope_var_iter";
      }]
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
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["path_map"; "uident_map"];
      name = "primitive_typ_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["path_iter"; "uident_iter"];
        name = "primitive_typ_iter";
      }]

type base_typ_data =
  | Primitive of primitive_typ
  | Collection of base_typ_data Mark.pos
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["Mark.pos_map"; "primitive_typ_map"];
      name = "base_typ_data_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["Mark.pos_iter"; "primitive_typ_iter"];
        name = "base_typ_data_iter";
      }]

type base_typ = Condition | Data of base_typ_data
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["base_typ_data_map"];
      name = "base_typ_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = ["base_typ_data_iter"];
        name = "base_typ_iter";
        nude = true;
      }]

type func_typ = {
  arg_typ : (lident Mark.pos * base_typ Mark.pos) list;
  return_typ : base_typ Mark.pos;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["lident_map"; "base_typ_map"];
      name = "func_typ_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = ["lident_iter"; "base_typ_iter"];
        name = "func_typ_iter";
        nude = true;
      }]

type typ = naked_typ Mark.pos

and naked_typ = Base of base_typ | Func of func_typ
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["func_typ_map"];
      name = "typ_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = ["func_typ_iter"];
        name = "typ_iter";
        nude = true;
      }]

type struct_decl_field = {
  struct_decl_field_name : lident Mark.pos;
  struct_decl_field_typ : typ;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["typ_map"; "lident_map"];
      name = "struct_decl_field_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["typ_iter"; "lident_iter"];
        name = "struct_decl_field_iter";
      }]

type struct_decl = {
  struct_decl_name : uident Mark.pos;
  struct_decl_fields : struct_decl_field Mark.pos list;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["struct_decl_field_map"];
      name = "struct_decl_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["struct_decl_field_iter"];
        name = "struct_decl_iter";
      }]

type enum_decl_case = {
  enum_decl_case_name : uident Mark.pos;
  enum_decl_case_typ : typ option;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["typ_map"];
      name = "enum_decl_case_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = ["typ_iter"];
        name = "enum_decl_case_iter";
        nude = true;
      }]

type enum_decl = {
  enum_decl_name : uident Mark.pos;
  enum_decl_cases : enum_decl_case Mark.pos list;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["enum_decl_case_map"];
      name = "enum_decl_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = ["enum_decl_case_iter"];
        name = "enum_decl_iter";
        nude = true;
      }]

type match_case_pattern =
  (path * uident Mark.pos) Mark.pos list * lident Mark.pos option
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["path_map"; "lident_map"; "uident_map"; "Mark.pos_map"];
      name = "match_case_pattern_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["path_iter"; "lident_iter"; "uident_iter"; "Mark.pos_iter"];
        name = "match_case_pattern_iter";
      }]

type op_kind = KPoly | KInt | KDec | KMoney | KDate | KDuration
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
      ancestors = ["op_kind_map"];
      name = "binop_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = ["op_kind_iter"];
        name = "binop_iter";
        nude = true;
      }]

type unop = Not | Minus of op_kind
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["op_kind_map"];
      name = "unop_map";
      nude = true;
    },
    visitors
      {
        variety = "iter";
        ancestors = ["op_kind_iter"];
        name = "unop_iter";
        nude = true;
      }]

type builtin_expression =
  | Cardinal
  | ToDecimal
  | ToMoney
  | GetDay
  | GetMonth
  | GetYear
  | LastDayOfMonth
  | FirstDayOfMonth
  | Round
[@@deriving
  visitors { variety = "map"; name = "builtin_expression_map"; nude = true },
    visitors { variety = "iter"; name = "builtin_expression_iter"; nude = true }]

type literal_date = {
  literal_date_day : (int[@opaque]);
  literal_date_month : (int[@opaque]);
  literal_date_year : (int[@opaque]);
}
[@@deriving
  visitors
    { variety = "map"; ancestors = ["Mark.pos_map"]; name = "literal_date_map" },
    visitors
      {
        variety = "iter";
        ancestors = ["Mark.pos_iter"];
        name = "literal_date_iter";
      }]

type literal_number =
  | Int of (string[@opaque])
  | Dec of (string[@opaque]) * (string[@opaque])
[@@deriving
  visitors { variety = "map"; name = "literal_number_map"; nude = true },
    visitors { variety = "iter"; name = "literal_number_iter"; nude = true }]

type literal_unit = Percent | Year | Month | Day
[@@deriving
  visitors { variety = "map"; name = "literal_unit_map"; nude = true },
    visitors { variety = "iter"; name = "literal_unit_iter"; nude = true }]

type money_amount = {
  money_amount_units : (string[@opaque]);
  money_amount_cents : (string[@opaque]);
}
[@@deriving
  visitors { variety = "map"; name = "money_amount_map"; nude = true },
    visitors { variety = "iter"; name = "money_amount_iter"; nude = true }]

type literal =
  | LNumber of literal_number Mark.pos * literal_unit Mark.pos option
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

type collection_op =
  | Exists of { predicate : lident Mark.pos * expression }
  | Forall of { predicate : lident Mark.pos * expression }
  | Map of { f : lident Mark.pos * expression }
  | Filter of { f : lident Mark.pos * expression }
  | AggregateSum of { typ : primitive_typ }
  (* it would be nice to remove the need for specifying the type here like for
     extremums, but we need an additionl overload for "neutral element for
     addition across types" *)
  | AggregateExtremum of { max : bool; default : expression }
  | AggregateArgExtremum of {
      max : bool;
      default : expression;
      f : lident Mark.pos * expression;
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
  | CollectionOp of collection_op * expression
  | MemCollection of expression * expression
  | TestMatchCase of expression * match_case_pattern Mark.pos
  | FunCall of expression * expression list
  | ScopeCall of
      (path * uident Mark.pos) Mark.pos * (lident Mark.pos * expression) list
  | LetIn of lident Mark.pos * expression * expression
  | Builtin of builtin_expression
  | Literal of literal
  | EnumInject of (path * uident Mark.pos) Mark.pos * expression option
  | StructLit of
      (path * uident Mark.pos) Mark.pos * (lident Mark.pos * expression) list
  | ArrayLit of expression list
  | Ident of path * lident Mark.pos
  | Dotted of expression * (path * lident Mark.pos) Mark.pos
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
  | ExceptionToLabel of lident Mark.pos
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["lident_map"; "Mark.pos_map"];
      name = "exception_to_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["lident_iter"; "Mark.pos_iter"];
        name = "exception_to_iter";
      }]

type rule = {
  rule_label : lident Mark.pos option;
  rule_exception_to : exception_to;
  rule_parameter : lident Mark.pos list Mark.pos option;
  rule_condition : expression option;
  rule_name : scope_var Mark.pos;
  rule_id : Shared_ast.RuleName.t; [@opaque]
  rule_consequence : (bool[@opaque]) Mark.pos;
  rule_state : lident Mark.pos option;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["expression_map"; "scope_var_map"; "exception_to_map"];
      name = "rule_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["expression_iter"; "scope_var_iter"; "exception_to_iter"];
        name = "rule_iter";
      }]

type definition = {
  definition_label : lident Mark.pos option;
  definition_exception_to : exception_to;
  definition_name : scope_var Mark.pos;
  definition_parameter : lident Mark.pos list Mark.pos option;
  definition_condition : expression option;
  definition_id : Shared_ast.RuleName.t; [@opaque]
  definition_expr : expression;
  definition_state : lident Mark.pos option;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["expression_map"; "scope_var_map"; "exception_to_map"];
      name = "definition_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["expression_iter"; "scope_var_iter"; "exception_to_iter"];
        name = "definition_iter";
      }]

type variation_typ = Increasing | Decreasing
[@@deriving
  visitors { variety = "map"; name = "variation_typ_map" },
    visitors { variety = "iter"; name = "variation_typ_iter" }]

type meta_assertion =
  | FixedBy of scope_var Mark.pos * lident Mark.pos
  | VariesWith of
      scope_var Mark.pos * expression * variation_typ Mark.pos option
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["variation_typ_map"; "scope_var_map"; "expression_map"];
      name = "meta_assertion_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["variation_typ_iter"; "scope_var_iter"; "expression_iter"];
        name = "meta_assertion_iter";
      }]

type assertion = {
  assertion_condition : expression option;
  assertion_content : expression;
}
[@@deriving
  visitors
    { variety = "map"; ancestors = ["expression_map"]; name = "assertion_map" },
    visitors
      {
        variety = "iter";
        ancestors = ["expression_iter"];
        name = "assertion_iter";
      }]

type scope_use_item =
  | Rule of rule
  | Definition of definition
  | Assertion of assertion
  | MetaAssertion of meta_assertion
  | DateRounding of variation_typ Mark.pos
[@@deriving
  visitors
    {
      variety = "map";
      ancestors =
        ["meta_assertion_map"; "definition_map"; "assertion_map"; "rule_map"];
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
  scope_use_condition : expression option;
  scope_use_name : uident Mark.pos;
  scope_use_items : scope_use_item Mark.pos list;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["expression_map"; "scope_use_item_map"];
      name = "scope_use_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["expression_iter"; "scope_use_item_iter"];
        name = "scope_use_iter";
      }]

type io_input = Input | Context | Internal
[@@deriving
  visitors { variety = "map"; name = "io_input_map" },
    visitors { variety = "iter"; name = "io_input_iter" }]

type scope_decl_context_io = {
  scope_decl_context_io_input : io_input Mark.pos;
  scope_decl_context_io_output : bool Mark.pos;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["io_input_map"; "Mark.pos_map"];
      name = "scope_decl_context_io_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["io_input_iter"; "Mark.pos_iter"];
        name = "scope_decl_context_io_iter";
      }]

type scope_decl_context_scope = {
  scope_decl_context_scope_name : lident Mark.pos;
  scope_decl_context_scope_sub_scope : uident Mark.pos;
  scope_decl_context_scope_attribute : scope_decl_context_io;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors =
        [
          "lident_map"; "uident_map"; "scope_decl_context_io_map"; "Mark.pos_map";
        ];
      name = "scope_decl_context_scope_map";
    },
    visitors
      {
        variety = "iter";
        ancestors =
          [
            "lident_iter";
            "uident_iter";
            "scope_decl_context_io_iter";
            "Mark.pos_iter";
          ];
        name = "scope_decl_context_scope_iter";
      }]

type scope_decl_context_data = {
  scope_decl_context_item_name : lident Mark.pos;
  scope_decl_context_item_typ : typ;
  scope_decl_context_item_parameters :
    (lident Mark.pos * typ) list Mark.pos option;
  scope_decl_context_item_attribute : scope_decl_context_io;
  scope_decl_context_item_states : lident Mark.pos list;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["typ_map"; "scope_decl_context_io_map"; "lident_map"];
      name = "scope_decl_context_data_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["typ_iter"; "scope_decl_context_io_iter"; "lident_iter"];
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
        ["scope_decl_context_data_map"; "scope_decl_context_scope_map"];
      name = "scope_decl_context_item_map";
    },
    visitors
      {
        variety = "iter";
        ancestors =
          ["scope_decl_context_data_iter"; "scope_decl_context_scope_iter"];
        name = "scope_decl_context_item_iter";
      }]

type scope_decl = {
  scope_decl_name : uident Mark.pos;
  scope_decl_context : scope_decl_context_item Mark.pos list;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["scope_decl_context_item_map"];
      name = "scope_decl_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["scope_decl_context_item_iter"];
        name = "scope_decl_iter";
      }]

type top_def = {
  topdef_name : lident Mark.pos;
  topdef_args : (lident Mark.pos * base_typ Mark.pos) list Mark.pos option;
      (** Empty list if this is not a function *)
  topdef_type : typ;
  topdef_expr : expression;
}
[@@deriving
  visitors
    {
      variety = "map";
      ancestors = ["lident_map"; "typ_map"; "expression_map"];
      name = "top_def_map";
    },
    visitors
      {
        variety = "iter";
        ancestors = ["lident_iter"; "typ_iter"; "expression_iter"];
        name = "top_def_iter";
      }]

type code_item =
  | ScopeUse of scope_use
  | ScopeDecl of scope_decl
  | StructDecl of struct_decl
  | EnumDecl of enum_decl
  | Topdef of top_def
[@@deriving
  visitors
    {
      variety = "map";
      ancestors =
        [
          "scope_decl_map";
          "enum_decl_map";
          "struct_decl_map";
          "scope_use_map";
          "top_def_map";
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
            "top_def_iter";
          ];
        name = "code_item_iter";
      }]

type code_block = code_item Mark.pos list
[@@deriving
  visitors
    { variety = "map"; ancestors = ["code_item_map"]; name = "code_block_map" },
    visitors
      {
        variety = "iter";
        ancestors = ["code_item_iter"];
        name = "code_block_iter";
      }]

type source_repr = (string[@opaque]) Mark.pos
[@@deriving
  visitors
    { variety = "map"; ancestors = ["Mark.pos_map"]; name = "source_repr_map" },
    visitors
      {
        variety = "iter";
        ancestors = ["Mark.pos_iter"];
        name = "source_repr_iter";
      }]

type law_heading = {
  law_heading_name : (string[@opaque]) Mark.pos;
  law_heading_id : (string[@opaque]) option;
  law_heading_is_archive : bool; [@opaque]
  law_heading_precedence : (int[@opaque]);
}
[@@deriving
  visitors
    { variety = "map"; ancestors = ["Mark.pos_map"]; name = "law_heading_map" },
    visitors
      {
        variety = "iter";
        ancestors = ["Mark.pos_iter"];
        name = "law_heading_iter";
      }]

type law_include =
  | PdfFile of (string[@opaque]) Mark.pos * (int[@opaque]) option
  | CatalaFile of (string[@opaque]) Mark.pos
  | LegislativeText of (string[@opaque]) Mark.pos
[@@deriving
  visitors
    { variety = "map"; ancestors = ["Mark.pos_map"]; name = "law_include_map" },
    visitors
      {
        variety = "iter";
        ancestors = ["Mark.pos_iter"];
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
  program_interfaces :
    ((Shared_ast.Qident.path[@opaque]) * code_item Mark.pos) list;
  program_items : law_structure list;
  program_source_files : (string[@opaque]) list;
}
[@@deriving
  visitors
    { variety = "map"; ancestors = ["law_structure_map"]; name = "program_map" },
    visitors
      {
        variety = "iter";
        ancestors = ["law_structure_iter"];
        name = "program_iter";
      }]

type source_file = law_structure list

(** {1 Helpers}*)

(** Translates a {!type: rule} into the corresponding {!type: definition} *)
let rule_to_def (rule : rule) : definition =
  let consequence_expr = Literal (LBool (Mark.remove rule.rule_consequence)) in
  {
    definition_label = rule.rule_label;
    definition_exception_to = rule.rule_exception_to;
    definition_name = rule.rule_name;
    definition_parameter = rule.rule_parameter;
    definition_condition = rule.rule_condition;
    definition_id = rule.rule_id;
    definition_expr = consequence_expr, Mark.get rule.rule_consequence;
    definition_state = rule.rule_state;
  }

let type_from_args
    (args : (lident Mark.pos * base_typ Mark.pos) list Mark.pos option)
    (return_typ : base_typ Mark.pos) : typ =
  match args with
  | None -> Mark.map (fun r -> Base r) return_typ
  | Some (arg_typ, _) ->
    Mark.add (Mark.get return_typ) (Func { arg_typ; return_typ })
