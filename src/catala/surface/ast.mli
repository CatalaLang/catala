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

open Utils

(** {1 Type definitions} *)

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

type match_case_pattern =
  (constructor Pos.marked option * constructor Pos.marked) list * ident Pos.marked option

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
  literal_date_day : (int[@opaque]) Pos.marked;
  literal_date_month : (int[@opaque]) Pos.marked;
  literal_date_year : (int[@opaque]) Pos.marked;
}

type literal_number =
  | Int of (Runtime.integer[@opaque])
  | Dec of (Runtime.integer[@opaque]) * (Runtime.integer[@opaque])

type literal_unit = Percent | Year | Month | Day

type money_amount = {
  money_amount_units : (Runtime.integer[@opaque]);
  money_amount_cents : (Runtime.integer[@opaque]);
}

type literal =
  | LNumber of literal_number Pos.marked * literal_unit Pos.marked option
  | LBool of bool
  | LMoneyAmount of money_amount
  | LDate of literal_date

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
  | TestMatchCase of expression Pos.marked * match_case_pattern Pos.marked
  | FunCall of expression Pos.marked * expression Pos.marked
  | Builtin of builtin_expression
  | Literal of literal
  | EnumInject of
      constructor Pos.marked option * constructor Pos.marked * expression Pos.marked option
  | EnumProject of expression Pos.marked * constructor Pos.marked
  | StructLit of constructor Pos.marked * (ident Pos.marked * expression Pos.marked) list
  | ArrayLit of expression Pos.marked list
  | Ident of ident
  | Dotted of expression Pos.marked * constructor Pos.marked option * ident Pos.marked
      (** Dotted is for both struct field projection and sub-scope variables *)

type exception_to = NotAnException | UnlabeledException | ExceptionToLabel of ident Pos.marked

type rule = {
  rule_label : ident Pos.marked option;
  rule_exception_to : exception_to;
  rule_parameter : ident Pos.marked option;
  rule_condition : expression Pos.marked option;
  rule_name : qident Pos.marked;
  rule_consequence : (bool[@opaque]) Pos.marked;
}

type definition = {
  definition_label : ident Pos.marked option;
  definition_exception_to : exception_to;
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

type source_repr = (string[@opaque]) Pos.marked

type law_article = {
  law_article_name : (string[@opaque]) Pos.marked;
  law_article_id : (string[@opaque]) option;
  law_article_expiration_date : (string[@opaque]) option;
  law_article_precedence : (int[@opaque]);
}

type law_include =
  | PdfFile of (string[@opaque]) Pos.marked * (int[@opaque]) option
  | CatalaFile of (string[@opaque]) Pos.marked
  | LegislativeText of (string[@opaque]) Pos.marked

type law_article_item = LawText of (string[@opaque]) | CodeBlock of code_block * source_repr

type law_heading = { law_heading_name : (string[@opaque]); law_heading_precedence : (int[@opaque]) }

type law_structure =
  | LawInclude of law_include
  | LawHeading of law_heading * law_structure list
  | LawArticle of law_article * law_article_item list
  | MetadataBlock of code_block * source_repr
  | IntermediateText of (string[@opaque])

type program_item = LawStructure of law_structure

type program = { program_items : program_item list; program_source_files : (string[@opaque]) list }

type source_file_or_master =
  | SourceFile of program_item list
  | MasterFile of string Pos.marked list

(** {1 Visitor classes for programs} *)

(** To allow for quick traversal and/or modification of this AST structure, we provide a
    {{:https://en.wikipedia.org/wiki/Visitor_pattern} visitor design pattern}. This feature is
    implemented via {{:https://gitlab.inria.fr/fpottier/visitors} François Pottier's OCaml visitors
    library}. *)

(** {2 Program map visitor} *)

class virtual ['self] program_map :
  object ('self)
    method visit_Add : 'monomorphic. 'env -> op_kind -> binop

    method visit_Aggregate : 'monomorphic. 'env -> aggregate_func -> collection_op

    method visit_AggregateArgExtremum :
      'monomorphic. 'env -> bool -> primitive_typ -> expression Pos.marked -> aggregate_func

    method visit_AggregateCount : 'monomorphic. 'env -> aggregate_func

    method visit_AggregateExtremum :
      'monomorphic. 'env -> bool -> primitive_typ -> expression Pos.marked -> aggregate_func

    method visit_AggregateSum : 'monomorphic. 'env -> primitive_typ -> aggregate_func

    method visit_And : 'monomorphic. 'env -> binop

    method visit_ArrayLit : 'monomorphic. 'env -> expression Pos.marked list -> expression

    method visit_Assertion : 'monomorphic. 'env -> assertion -> scope_use_item

    method visit_Base : 'monomorphic. 'env -> base_typ -> typ

    method visit_Binop :
      'monomorphic. 'env -> binop Pos.marked -> expression Pos.marked -> expression Pos.marked ->
      expression

    method visit_Boolean : 'monomorphic. 'env -> primitive_typ

    method visit_Builtin : 'monomorphic. 'env -> builtin_expression -> expression

    method visit_Cardinal : 'monomorphic. 'env -> builtin_expression

    method visit_CatalaFile : 'monomorphic. 'env -> string Pos.marked -> law_include

    method visit_CodeBlock :
      'monomorphic. 'env -> code_block -> string Pos.marked -> law_article_item

    method visit_Collection : 'monomorphic. 'env -> base_typ_data Pos.marked -> base_typ_data

    method visit_CollectionOp :
      'monomorphic. 'env -> collection_op Pos.marked -> ident Pos.marked -> expression Pos.marked ->
      expression Pos.marked -> expression

    method visit_Condition : 'monomorphic. 'env -> base_typ

    method visit_ContextData :
      'monomorphic. 'env -> scope_decl_context_data -> scope_decl_context_item

    method visit_ContextScope :
      'monomorphic. 'env -> scope_decl_context_scope -> scope_decl_context_item

    method visit_Data : 'monomorphic. 'env -> base_typ_data -> base_typ

    method visit_Date : 'monomorphic. 'env -> primitive_typ

    method visit_Day : 'monomorphic. 'env -> literal_unit

    method visit_Dec : 'monomorphic. 'env -> Runtime.integer -> Runtime.integer -> literal_number

    method visit_Decimal : 'monomorphic. 'env -> primitive_typ

    method visit_Decreasing : 'monomorphic. 'env -> variation_typ

    method visit_Definition : 'monomorphic. 'env -> definition -> scope_use_item

    method visit_Div : 'monomorphic. 'env -> op_kind -> binop

    method visit_Dotted :
      'monomorphic. 'env -> expression Pos.marked -> constructor Pos.marked option ->
      ident Pos.marked -> expression

    method visit_Duration : 'monomorphic. 'env -> primitive_typ

    method visit_EnumDecl : 'monomorphic. 'env -> enum_decl -> code_item

    method visit_EnumInject :
      'monomorphic. 'env -> constructor Pos.marked option -> constructor Pos.marked ->
      expression Pos.marked option -> expression

    method visit_EnumProject :
      'monomorphic. 'env -> expression Pos.marked -> constructor Pos.marked -> expression

    method visit_Eq : 'monomorphic. 'env -> binop

    method visit_ExceptionToLabel : 'monomorphic. 'env -> ident Pos.marked -> exception_to

    method visit_Exists : 'monomorphic. 'env -> collection_op

    method visit_Filter : 'monomorphic. 'env -> collection_op

    method visit_FixedBy :
      'monomorphic. 'env -> qident Pos.marked -> ident Pos.marked -> meta_assertion

    method visit_Forall : 'monomorphic. 'env -> collection_op

    method visit_FunCall :
      'monomorphic. 'env -> expression Pos.marked -> expression Pos.marked -> expression

    method visit_Func : 'monomorphic. 'env -> func_typ -> typ

    method visit_GetDay : 'monomorphic. 'env -> builtin_expression

    method visit_GetMonth : 'monomorphic. 'env -> builtin_expression

    method visit_GetYear : 'monomorphic. 'env -> builtin_expression

    method visit_Gt : 'monomorphic. 'env -> op_kind -> binop

    method visit_Gte : 'monomorphic. 'env -> op_kind -> binop

    method visit_Ident : 'monomorphic. 'env -> ident -> expression

    method visit_IfThenElse :
      'monomorphic. 'env -> expression Pos.marked -> expression Pos.marked ->
      expression Pos.marked -> expression

    method visit_Increasing : 'monomorphic. 'env -> variation_typ

    method visit_Int : 'monomorphic. 'env -> Runtime.integer -> literal_number

    method visit_IntToDec : 'monomorphic. 'env -> builtin_expression

    method visit_Integer : 'monomorphic. 'env -> primitive_typ

    method visit_IntermediateText : 'monomorphic. 'env -> string -> law_structure

    method visit_KDate : 'monomorphic. 'env -> op_kind

    method visit_KDec : 'monomorphic. 'env -> op_kind

    method visit_KDuration : 'monomorphic. 'env -> op_kind

    method visit_KInt : 'monomorphic. 'env -> op_kind

    method visit_KMoney : 'monomorphic. 'env -> op_kind

    method visit_LBool : 'monomorphic. 'env -> bool -> literal

    method visit_LDate : 'monomorphic. 'env -> literal_date -> literal

    method visit_LMoneyAmount : 'monomorphic. 'env -> money_amount -> literal

    method visit_LNumber :
      'monomorphic. 'env -> literal_number Pos.marked -> literal_unit Pos.marked option -> literal

    method visit_LawArticle :
      'monomorphic. 'env -> law_article -> law_article_item list -> law_structure

    method visit_LawHeading :
      'monomorphic. 'env -> law_heading -> law_structure list -> law_structure

    method visit_LawInclude : 'monomorphic. 'env -> law_include -> law_structure

    method visit_LawStructure : 'monomorphic. 'env -> law_structure -> program_item

    method visit_LawText : 'monomorphic. 'env -> string -> law_article_item

    method visit_LegislativeText : 'monomorphic. 'env -> string Pos.marked -> law_include

    method visit_Literal : 'monomorphic. 'env -> literal -> expression

    method visit_Lt : 'monomorphic. 'env -> op_kind -> binop

    method visit_Lte : 'monomorphic. 'env -> op_kind -> binop

    method visit_Map : 'monomorphic. 'env -> collection_op

    method visit_MatchWith :
      'monomorphic. 'env -> expression Pos.marked -> match_cases Pos.marked -> expression

    method visit_MemCollection :
      'monomorphic. 'env -> expression Pos.marked -> expression Pos.marked -> expression

    method visit_MetaAssertion : 'monomorphic. 'env -> meta_assertion -> scope_use_item

    method visit_MetadataBlock :
      'monomorphic. 'env -> code_block -> string Pos.marked -> law_structure

    method visit_Minus : 'monomorphic. 'env -> op_kind -> unop

    method visit_Money : 'monomorphic. 'env -> primitive_typ

    method visit_Month : 'monomorphic. 'env -> literal_unit

    method visit_Mult : 'monomorphic. 'env -> op_kind -> binop

    method visit_Named : 'monomorphic. 'env -> constructor -> primitive_typ

    method visit_Neq : 'monomorphic. 'env -> binop

    method visit_Not : 'monomorphic. 'env -> unop

    method visit_NotAnException : 'monomorphic. 'env -> exception_to

    method visit_Or : 'monomorphic. 'env -> binop

    method visit_PdfFile : 'monomorphic. 'env -> string Pos.marked -> int option -> law_include

    method visit_Percent : 'monomorphic. 'env -> literal_unit

    method visit_Primitive : 'monomorphic. 'env -> primitive_typ -> base_typ_data

    method visit_Rule : 'monomorphic. 'env -> rule -> scope_use_item

    method visit_ScopeDecl : 'monomorphic. 'env -> scope_decl -> code_item

    method visit_ScopeUse : 'monomorphic. 'env -> scope_use -> code_item

    method visit_StructDecl : 'monomorphic. 'env -> struct_decl -> code_item

    method visit_StructLit :
      'monomorphic. 'env -> constructor Pos.marked ->
      (ident Pos.marked * expression Pos.marked) list -> expression

    method visit_Sub : 'monomorphic. 'env -> op_kind -> binop

    method visit_TestMatchCase :
      'monomorphic. 'env -> expression Pos.marked -> match_case_pattern Pos.marked -> expression

    method visit_Text : 'monomorphic. 'env -> primitive_typ

    method visit_UnlabeledException : 'monomorphic. 'env -> exception_to

    method visit_Unop : 'monomorphic. 'env -> unop Pos.marked -> expression Pos.marked -> expression

    method visit_VariesWith :
      'monomorphic. 'env -> qident Pos.marked -> expression Pos.marked ->
      variation_typ Pos.marked option -> meta_assertion

    method visit_Year : 'monomorphic. 'env -> literal_unit

    method visit_aggregate_func : 'monomorphic. 'env -> aggregate_func -> aggregate_func

    method visit_assertion : 'monomorphic. 'env -> assertion -> assertion

    method visit_base_typ : 'monomorphic. 'env -> base_typ -> base_typ

    method visit_base_typ_data : 'monomorphic. 'env -> base_typ_data -> base_typ_data

    method visit_binop : 'monomorphic. 'env -> binop -> binop

    method visit_builtin_expression : 'monomorphic. 'env -> builtin_expression -> builtin_expression

    method visit_code_block :
      'monomorphic. 'env -> code_item Pos.marked list -> code_item Pos.marked list

    method visit_code_item : 'monomorphic. 'env -> code_item -> code_item

    method visit_collection_op : 'monomorphic. 'env -> collection_op -> collection_op

    method visit_constructor : 'monomorphic. 'env -> constructor -> constructor

    method visit_definition : 'monomorphic. 'env -> definition -> definition

    method visit_enum_decl : 'monomorphic. 'env -> enum_decl -> enum_decl

    method visit_enum_decl_case : 'monomorphic. 'env -> enum_decl_case -> enum_decl_case

    method visit_exception_to : 'monomorphic. 'env -> exception_to -> exception_to

    method visit_expression : 'monomorphic. 'env -> expression -> expression

    method visit_func_typ : 'monomorphic. 'env -> func_typ -> func_typ

    method visit_ident : 'monomorphic. 'env -> ident -> ident

    method visit_law_article : 'monomorphic. 'env -> law_article -> law_article

    method visit_law_article_item : 'monomorphic. 'env -> law_article_item -> law_article_item

    method visit_law_heading : 'monomorphic. 'env -> law_heading -> law_heading

    method visit_law_include : 'monomorphic. 'env -> law_include -> law_include

    method visit_law_structure : 'monomorphic. 'env -> law_structure -> law_structure

    method visit_literal : 'monomorphic. 'env -> literal -> literal

    method visit_literal_date : 'monomorphic. 'env -> literal_date -> literal_date

    method visit_literal_number : 'monomorphic. 'env -> literal_number -> literal_number

    method visit_literal_unit : 'monomorphic. 'env -> literal_unit -> literal_unit

    method visit_marked : 'a. ('env -> 'a -> 'a) -> 'env -> 'a Pos.marked -> 'a Pos.marked

    method visit_match_case : 'monomorphic. 'env -> match_case -> match_case

    method visit_match_case_pattern :
      'monomorphic. 'env ->
      (constructor Pos.marked option * constructor Pos.marked) list * ident Pos.marked option ->
      (constructor Pos.marked option * constructor Pos.marked) list * ident Pos.marked option

    method visit_match_cases : 'monomorphic. 'env -> match_cases -> match_cases

    method visit_meta_assertion : 'monomorphic. 'env -> meta_assertion -> meta_assertion

    method visit_money_amount : 'monomorphic. 'env -> money_amount -> money_amount

    method visit_op_kind : 'monomorphic. 'env -> op_kind -> op_kind

    method visit_primitive_typ : 'monomorphic. 'env -> primitive_typ -> primitive_typ

    method visit_program : 'monomorphic. 'env -> program -> program

    method visit_program_item : 'monomorphic. 'env -> program_item -> program_item

    method visit_qident : 'monomorphic. 'env -> ident Pos.marked list -> ident Pos.marked list

    method visit_rule : 'monomorphic. 'env -> rule -> rule

    method visit_scope_decl : 'monomorphic. 'env -> scope_decl -> scope_decl

    method visit_scope_decl_context_data :
      'monomorphic. 'env -> scope_decl_context_data -> scope_decl_context_data

    method visit_scope_decl_context_item :
      'monomorphic. 'env -> scope_decl_context_item -> scope_decl_context_item

    method visit_scope_decl_context_scope :
      'monomorphic. 'env -> scope_decl_context_scope -> scope_decl_context_scope

    method visit_scope_use : 'monomorphic. 'env -> scope_use -> scope_use

    method visit_scope_use_item : 'monomorphic. 'env -> scope_use_item -> scope_use_item

    method visit_source_repr : 'monomorphic. 'env -> string Pos.marked -> string Pos.marked

    method visit_struct_decl : 'monomorphic. 'env -> struct_decl -> struct_decl

    method visit_struct_decl_field : 'monomorphic. 'env -> struct_decl_field -> struct_decl_field

    method visit_typ : 'monomorphic. 'env -> typ -> typ

    method visit_unop : 'monomorphic. 'env -> unop -> unop

    method visit_variation_typ : 'monomorphic. 'env -> variation_typ -> variation_typ
  end

(** {2 Program iter visitor} *)

class virtual ['self] program_iter :
  object ('self)
    method visit_Add : 'monomorphic. 'env -> op_kind -> unit

    method visit_Aggregate : 'monomorphic. 'env -> aggregate_func -> unit

    method visit_AggregateArgExtremum :
      'monomorphic. 'env -> bool -> primitive_typ -> expression Pos.marked -> unit

    method visit_AggregateCount : 'monomorphic. 'env -> unit

    method visit_AggregateExtremum :
      'monomorphic. 'env -> bool -> primitive_typ -> expression Pos.marked -> unit

    method visit_AggregateSum : 'monomorphic. 'env -> primitive_typ -> unit

    method visit_And : 'monomorphic. 'env -> unit

    method visit_ArrayLit : 'monomorphic. 'env -> expression Pos.marked list -> unit

    method visit_Assertion : 'monomorphic. 'env -> assertion -> unit

    method visit_Base : 'monomorphic. 'env -> base_typ -> unit

    method visit_Binop :
      'monomorphic. 'env -> binop Pos.marked -> expression Pos.marked -> expression Pos.marked ->
      unit

    method visit_Boolean : 'monomorphic. 'env -> unit

    method visit_Builtin : 'monomorphic. 'env -> builtin_expression -> unit

    method visit_Cardinal : 'monomorphic. 'env -> unit

    method visit_CatalaFile : 'monomorphic. 'env -> string Pos.marked -> unit

    method visit_CodeBlock : 'monomorphic. 'env -> code_block -> string Pos.marked -> unit

    method visit_Collection : 'monomorphic. 'env -> base_typ_data Pos.marked -> unit

    method visit_CollectionOp :
      'monomorphic. 'env -> collection_op Pos.marked -> ident Pos.marked -> expression Pos.marked ->
      expression Pos.marked -> unit

    method visit_Condition : 'monomorphic. 'env -> unit

    method visit_ContextData : 'monomorphic. 'env -> scope_decl_context_data -> unit

    method visit_ContextScope : 'monomorphic. 'env -> scope_decl_context_scope -> unit

    method visit_Data : 'monomorphic. 'env -> base_typ_data -> unit

    method visit_Date : 'monomorphic. 'env -> unit

    method visit_Day : 'monomorphic. 'env -> unit

    method visit_Dec : 'monomorphic. 'env -> Runtime.integer -> Runtime.integer -> unit

    method visit_Decimal : 'monomorphic. 'env -> unit

    method visit_Decreasing : 'monomorphic. 'env -> unit

    method visit_Definition : 'monomorphic. 'env -> definition -> unit

    method visit_Div : 'monomorphic. 'env -> op_kind -> unit

    method visit_Dotted :
      'monomorphic. 'env -> expression Pos.marked -> constructor Pos.marked option ->
      ident Pos.marked -> unit

    method visit_Duration : 'monomorphic. 'env -> unit

    method visit_EnumDecl : 'monomorphic. 'env -> enum_decl -> unit

    method visit_EnumInject :
      'monomorphic. 'env -> constructor Pos.marked option -> constructor Pos.marked ->
      expression Pos.marked option -> unit

    method visit_EnumProject :
      'monomorphic. 'env -> expression Pos.marked -> constructor Pos.marked -> unit

    method visit_Eq : 'monomorphic. 'env -> unit

    method visit_ExceptionToLabel : 'monomorphic. 'env -> ident Pos.marked -> unit

    method visit_Exists : 'monomorphic. 'env -> unit

    method visit_Filter : 'monomorphic. 'env -> unit

    method visit_FixedBy : 'monomorphic. 'env -> qident Pos.marked -> ident Pos.marked -> unit

    method visit_Forall : 'monomorphic. 'env -> unit

    method visit_FunCall :
      'monomorphic. 'env -> expression Pos.marked -> expression Pos.marked -> unit

    method visit_Func : 'monomorphic. 'env -> func_typ -> unit

    method visit_GetDay : 'monomorphic. 'env -> unit

    method visit_GetMonth : 'monomorphic. 'env -> unit

    method visit_GetYear : 'monomorphic. 'env -> unit

    method visit_Gt : 'monomorphic. 'env -> op_kind -> unit

    method visit_Gte : 'monomorphic. 'env -> op_kind -> unit

    method visit_Ident : 'monomorphic. 'env -> ident -> unit

    method visit_IfThenElse :
      'monomorphic. 'env -> expression Pos.marked -> expression Pos.marked ->
      expression Pos.marked -> unit

    method visit_Increasing : 'monomorphic. 'env -> unit

    method visit_Int : 'monomorphic. 'env -> Runtime.integer -> unit

    method visit_IntToDec : 'monomorphic. 'env -> unit

    method visit_Integer : 'monomorphic. 'env -> unit

    method visit_IntermediateText : 'monomorphic. 'env -> string -> unit

    method visit_KDate : 'monomorphic. 'env -> unit

    method visit_KDec : 'monomorphic. 'env -> unit

    method visit_KDuration : 'monomorphic. 'env -> unit

    method visit_KInt : 'monomorphic. 'env -> unit

    method visit_KMoney : 'monomorphic. 'env -> unit

    method visit_LBool : 'monomorphic. 'env -> bool -> unit

    method visit_LDate : 'monomorphic. 'env -> literal_date -> unit

    method visit_LMoneyAmount : 'monomorphic. 'env -> money_amount -> unit

    method visit_LNumber :
      'monomorphic. 'env -> literal_number Pos.marked -> literal_unit Pos.marked option -> unit

    method visit_LawArticle : 'monomorphic. 'env -> law_article -> law_article_item list -> unit

    method visit_LawHeading : 'monomorphic. 'env -> law_heading -> law_structure list -> unit

    method visit_LawInclude : 'monomorphic. 'env -> law_include -> unit

    method visit_LawStructure : 'monomorphic. 'env -> law_structure -> unit

    method visit_LawText : 'monomorphic. 'env -> string -> unit

    method visit_LegislativeText : 'monomorphic. 'env -> string Pos.marked -> unit

    method visit_Literal : 'monomorphic. 'env -> literal -> unit

    method visit_Lt : 'monomorphic. 'env -> op_kind -> unit

    method visit_Lte : 'monomorphic. 'env -> op_kind -> unit

    method visit_Map : 'monomorphic. 'env -> unit

    method visit_MatchWith :
      'monomorphic. 'env -> expression Pos.marked -> match_cases Pos.marked -> unit

    method visit_MemCollection :
      'monomorphic. 'env -> expression Pos.marked -> expression Pos.marked -> unit

    method visit_MetaAssertion : 'monomorphic. 'env -> meta_assertion -> unit

    method visit_MetadataBlock : 'monomorphic. 'env -> code_block -> string Pos.marked -> unit

    method visit_Minus : 'monomorphic. 'env -> op_kind -> unit

    method visit_Money : 'monomorphic. 'env -> unit

    method visit_Month : 'monomorphic. 'env -> unit

    method visit_Mult : 'monomorphic. 'env -> op_kind -> unit

    method visit_Named : 'monomorphic. 'env -> constructor -> unit

    method visit_Neq : 'monomorphic. 'env -> unit

    method visit_Not : 'monomorphic. 'env -> unit

    method visit_NotAnException : 'monomorphic. 'env -> unit

    method visit_Or : 'monomorphic. 'env -> unit

    method visit_PdfFile : 'monomorphic. 'env -> string Pos.marked -> int option -> unit

    method visit_Percent : 'monomorphic. 'env -> unit

    method visit_Primitive : 'monomorphic. 'env -> primitive_typ -> unit

    method visit_Rule : 'monomorphic. 'env -> rule -> unit

    method visit_ScopeDecl : 'monomorphic. 'env -> scope_decl -> unit

    method visit_ScopeUse : 'monomorphic. 'env -> scope_use -> unit

    method visit_StructDecl : 'monomorphic. 'env -> struct_decl -> unit

    method visit_StructLit :
      'monomorphic. 'env -> constructor Pos.marked ->
      (ident Pos.marked * expression Pos.marked) list -> unit

    method visit_Sub : 'monomorphic. 'env -> op_kind -> unit

    method visit_TestMatchCase :
      'monomorphic. 'env -> expression Pos.marked -> match_case_pattern Pos.marked -> unit

    method visit_Text : 'monomorphic. 'env -> unit

    method visit_UnlabeledException : 'monomorphic. 'env -> unit

    method visit_Unop : 'monomorphic. 'env -> unop Pos.marked -> expression Pos.marked -> unit

    method visit_VariesWith :
      'monomorphic. 'env -> qident Pos.marked -> expression Pos.marked ->
      variation_typ Pos.marked option -> unit

    method visit_Year : 'monomorphic. 'env -> unit

    method visit_aggregate_func : 'monomorphic. 'env -> aggregate_func -> unit

    method visit_assertion : 'monomorphic. 'env -> assertion -> unit

    method visit_base_typ : 'monomorphic. 'env -> base_typ -> unit

    method visit_base_typ_data : 'monomorphic. 'env -> base_typ_data -> unit

    method visit_binop : 'monomorphic. 'env -> binop -> unit

    method visit_builtin_expression : 'monomorphic. 'env -> builtin_expression -> unit

    method visit_code_block : 'monomorphic. 'env -> code_item Pos.marked list -> unit

    method visit_code_item : 'monomorphic. 'env -> code_item -> unit

    method visit_collection_op : 'monomorphic. 'env -> collection_op -> unit

    method visit_constructor : 'monomorphic. 'env -> constructor -> unit

    method visit_definition : 'monomorphic. 'env -> definition -> unit

    method visit_enum_decl : 'monomorphic. 'env -> enum_decl -> unit

    method visit_enum_decl_case : 'monomorphic. 'env -> enum_decl_case -> unit

    method visit_exception_to : 'monomorphic. 'env -> exception_to -> unit

    method visit_expression : 'monomorphic. 'env -> expression -> unit

    method visit_func_typ : 'monomorphic. 'env -> func_typ -> unit

    method visit_ident : 'monomorphic. 'env -> ident -> unit

    method visit_law_article : 'monomorphic. 'env -> law_article -> unit

    method visit_law_article_item : 'monomorphic. 'env -> law_article_item -> unit

    method visit_law_heading : 'monomorphic. 'env -> law_heading -> unit

    method visit_law_include : 'monomorphic. 'env -> law_include -> unit

    method visit_law_structure : 'monomorphic. 'env -> law_structure -> unit

    method visit_literal : 'monomorphic. 'env -> literal -> unit

    method visit_literal_date : 'monomorphic. 'env -> literal_date -> unit

    method visit_literal_number : 'monomorphic. 'env -> literal_number -> unit

    method visit_literal_unit : 'monomorphic. 'env -> literal_unit -> unit

    method visit_marked : 'a. ('env -> 'a -> unit) -> 'env -> 'a Pos.marked -> unit

    method visit_match_case : 'monomorphic. 'env -> match_case -> unit

    method visit_match_case_pattern :
      'monomorphic. 'env ->
      (constructor Pos.marked option * constructor Pos.marked) list * ident Pos.marked option ->
      unit

    method visit_match_cases : 'monomorphic. 'env -> match_cases -> unit

    method visit_meta_assertion : 'monomorphic. 'env -> meta_assertion -> unit

    method visit_money_amount : 'monomorphic. 'env -> money_amount -> unit

    method visit_op_kind : 'monomorphic. 'env -> op_kind -> unit

    method visit_primitive_typ : 'monomorphic. 'env -> primitive_typ -> unit

    method visit_program : 'monomorphic. 'env -> program -> unit

    method visit_program_item : 'monomorphic. 'env -> program_item -> unit

    method visit_qident : 'monomorphic. 'env -> ident Pos.marked list -> unit

    method visit_rule : 'monomorphic. 'env -> rule -> unit

    method visit_scope_decl : 'monomorphic. 'env -> scope_decl -> unit

    method visit_scope_decl_context_data : 'monomorphic. 'env -> scope_decl_context_data -> unit

    method visit_scope_decl_context_item : 'monomorphic. 'env -> scope_decl_context_item -> unit

    method visit_scope_decl_context_scope : 'monomorphic. 'env -> scope_decl_context_scope -> unit

    method visit_scope_use : 'monomorphic. 'env -> scope_use -> unit

    method visit_scope_use_item : 'monomorphic. 'env -> scope_use_item -> unit

    method visit_source_repr : 'monomorphic. 'env -> string Pos.marked -> unit

    method visit_struct_decl : 'monomorphic. 'env -> struct_decl -> unit

    method visit_struct_decl_field : 'monomorphic. 'env -> struct_decl_field -> unit

    method visit_typ : 'monomorphic. 'env -> typ -> unit

    method visit_unop : 'monomorphic. 'env -> unop -> unit

    method visit_variation_typ : 'monomorphic. 'env -> variation_typ -> unit
  end
