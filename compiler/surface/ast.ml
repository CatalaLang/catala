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
(** Constructors are CamelCase *)

and lident = (string[@opaque])
(** Idents are snake_case *)

and path = uident Mark.pos list

and scope_var = lident Mark.pos list
(** [foo.bar] in binding position: used to specify variables of subscopes *)

and primitive_typ =
  | Integer
  | Decimal
  | Boolean
  | Money
  | Duration
  | Text
  | Date
  | Named of path * uident Mark.pos

and base_typ_data =
  | Primitive of primitive_typ
  | Collection of base_typ_data Mark.pos

and base_typ = Condition | Data of base_typ_data

and func_typ = {
  arg_typ : (lident Mark.pos * base_typ Mark.pos) list;
  return_typ : base_typ Mark.pos;
}

and typ = naked_typ Mark.pos
and naked_typ = Base of base_typ | Func of func_typ

and struct_decl_field = {
  struct_decl_field_name : lident Mark.pos;
  struct_decl_field_typ : typ;
}

and struct_decl = {
  struct_decl_name : uident Mark.pos;
  struct_decl_fields : struct_decl_field Mark.pos list;
}

and enum_decl_case = {
  enum_decl_case_name : uident Mark.pos;
  enum_decl_case_typ : typ option;
}

and enum_decl = {
  enum_decl_name : uident Mark.pos;
  enum_decl_cases : enum_decl_case Mark.pos list;
}

and match_case_pattern =
  (path * uident Mark.pos) Mark.pos list * lident Mark.pos option

and op_kind = KPoly | KInt | KDec | KMoney | KDate | KDuration

and binop =
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

and unop = Not | Minus of op_kind

and builtin_expression =
  | Cardinal
  | ToDecimal
  | ToMoney
  | GetDay
  | GetMonth
  | GetYear
  | LastDayOfMonth
  | FirstDayOfMonth
  | Round

and literal_date = {
  literal_date_day : (int[@opaque]);
  literal_date_month : (int[@opaque]);
  literal_date_year : (int[@opaque]);
}

and literal_number =
  | Int of (string[@opaque])
  | Dec of (string[@opaque]) * (string[@opaque])

and literal_unit = Percent | Year | Month | Day

and money_amount = {
  money_amount_units : (string[@opaque]);
  money_amount_cents : (string[@opaque]);
}

and literal =
  | LNumber of literal_number Mark.pos * literal_unit Mark.pos option
  | LBool of bool
  | LMoneyAmount of money_amount
  | LDate of literal_date

and collection_op =
  | Exists of { predicate : lident Mark.pos * expression }
  | Forall of { predicate : lident Mark.pos * expression }
  | Map of { f : lident Mark.pos * expression }
  | Filter of { f : lident Mark.pos * expression }
  | AggregateSum of { typ : primitive_typ }
  (* it would be nice to remove the need for specifying the and here like for
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

and exception_to =
  | NotAnException
  | UnlabeledException
  | ExceptionToLabel of lident Mark.pos

and rule = {
  rule_label : lident Mark.pos option;
  rule_exception_to : exception_to;
  rule_parameter : lident Mark.pos list Mark.pos option;
  rule_condition : expression option;
  rule_name : scope_var Mark.pos;
  rule_id : Shared_ast.RuleName.t; [@opaque]
  rule_consequence : (bool[@opaque]) Mark.pos;
  rule_state : lident Mark.pos option;
}

and definition = {
  definition_label : lident Mark.pos option;
  definition_exception_to : exception_to;
  definition_name : scope_var Mark.pos;
  definition_parameter : lident Mark.pos list Mark.pos option;
  definition_condition : expression option;
  definition_id : Shared_ast.RuleName.t; [@opaque]
  definition_expr : expression;
  definition_state : lident Mark.pos option;
}

and variation_typ = Increasing | Decreasing

and meta_assertion =
  | FixedBy of scope_var Mark.pos * lident Mark.pos
  | VariesWith of
      scope_var Mark.pos * expression * variation_typ Mark.pos option

and assertion = {
  assertion_condition : expression option;
  assertion_content : expression;
}

and scope_use_item =
  | Rule of rule
  | Definition of definition
  | Assertion of assertion
  | MetaAssertion of meta_assertion
  | DateRounding of variation_typ Mark.pos

and scope_use = {
  scope_use_condition : expression option;
  scope_use_name : uident Mark.pos;
  scope_use_items : scope_use_item Mark.pos list;
}

and io_input = Input | Context | Internal

and scope_decl_context_io = {
  scope_decl_context_io_input : io_input Mark.pos;
  scope_decl_context_io_output : bool Mark.pos;
}

and scope_decl_context_scope = {
  scope_decl_context_scope_name : lident Mark.pos;
  scope_decl_context_scope_sub_scope : (path * uident Mark.pos) Mark.pos;
  scope_decl_context_scope_attribute : scope_decl_context_io;
}

and scope_decl_context_data = {
  scope_decl_context_item_name : lident Mark.pos;
  scope_decl_context_item_typ : typ;
  scope_decl_context_item_parameters :
    (lident Mark.pos * typ) list Mark.pos option;
  scope_decl_context_item_attribute : scope_decl_context_io;
  scope_decl_context_item_states : lident Mark.pos list;
}

and scope_decl_context_item =
  | ContextData of scope_decl_context_data
  | ContextScope of scope_decl_context_scope

and scope_decl = {
  scope_decl_name : uident Mark.pos;
  scope_decl_context : scope_decl_context_item Mark.pos list;
}

and top_def = {
  topdef_name : lident Mark.pos;
  topdef_args : (lident Mark.pos * base_typ Mark.pos) list Mark.pos option;
      (** Empty list if this is not a function *)
  topdef_type : typ;
  topdef_expr : expression option;
}

and code_item =
  | ScopeUse of scope_use
  | ScopeDecl of scope_decl
  | StructDecl of struct_decl
  | EnumDecl of enum_decl
  | Topdef of top_def

and code_block = code_item Mark.pos list
and source_repr = (string[@opaque]) Mark.pos

and law_heading = {
  law_heading_name : (string[@opaque]) Mark.pos;
  law_heading_id : (string[@opaque]) option;
  law_heading_is_archive : bool; [@opaque]
  law_heading_precedence : (int[@opaque]);
}

and law_include =
  | PdfFile of (string[@opaque]) Mark.pos * (int[@opaque]) option
  | CatalaFile of (string[@opaque]) Mark.pos
  | LegislativeText of (string[@opaque]) Mark.pos

and law_structure =
  | LawInclude of law_include
  | LawHeading of law_heading * law_structure list
  | LawText of (string[@opaque])
  | CodeBlock of code_block * source_repr * bool (* Metadata if true *)

and interface = code_block
(** Invariant: an interface shall only contain [*Decl] elements, or [Topdef]
    elements with [topdef_expr = None] *)

and program = {
  program_items : law_structure list;
  program_source_files : (string[@opaque]) list;
  program_modules : (uident * interface) list;
}

and source_file = law_structure list
[@@deriving visitors { variety = "map"; ancestors = ["Mark.pos_map"] }]

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
