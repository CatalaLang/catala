(*
  This file is part of the Lawspec compiler, a specification language for tax and social benefits
  computation rules.
  Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

%{
  open Ast
  open Parse_utils
%}

%token EOF
%token<string> LAW_ARTICLE
%token<string> LAW_CODE
%token<string> LAW_TEXT
%token<string> CONSTRUCTOR IDENT
%token<string> END_CODE
%token<int> INT_LITERAL
%token<int * int> DECIMAL_LITERAL
%token BEGIN_CODE
%token COLON ALT DATA
%token OF INTEGER COLLECTION
%token RULE CONDITION DEFINED_AS
%token EXISTS IN SUCH THAT NOW LESSER GREATER
%token DOT AND OR LPAREN RPAREN OPTIONAL EQUAL
%token COMMA CARDINAL LESSER_EQUAL GREATER_EQUAL
%token ASSERTION FIXED BY YEAR
%token PLUS MINUS MULT DIV MATCH WITH VARIES_WITH
%token FOR ALL WE_HAVE INCREASING DECREASING
%token NOT BOOLEAN PERCENT
%token FIELD FILLED IFF EURO NOT_EQUAL DEFINITION
%token STRUCT CONTENT IF THEN DEPENDS DECLARATION
%token CONTEXT INCLUDES ENUM ELSE DATE SUM
%token BEGIN_METADATA END_METADATA MONEY DECIMAL

%type <Ast.source_file> source_file

%start source_file

%%

typ_base:
| INTEGER { (Integer, mk_position $sloc) }
| BOOLEAN { (Boolean, mk_position $sloc) }
| MONEY { (Money, mk_position $sloc) }
| DECIMAL { (Decimal, mk_position $sloc) }
| DATE { (Date, mk_position $sloc) }
| c = constructor {
  let (s, _) = c in
  (Named s, mk_position $sloc)
}

collection_marked:
| COLLECTION { mk_position $sloc }

optional_marked:
| OPTIONAL { mk_position $sloc }

typ:
| collection = option(collection_marked) t = typ_base optional = option(optional_marked) {
  (Data {
    typ_data_collection = collection;
    typ_data_optional = optional;
    typ_data_base = t;
  }, mk_position $sloc)
}

qident_late:
| ident {}
| constructor {}
| ident DOT qident_late {}
| constructor DOT qident_late {}

qident:
| ident {}
| ident DOT qident_late {}
| constructor DOT qident_late {}

primitive_expression:
| NOW {}
| qident {}
| LPAREN expression RPAREN {}

date_qualifier:
| YEAR {}

constructor_payload:
| OF base_expression {}

num_literal:
| INT_LITERAL {}
| DECIMAL_LITERAL {}

literal:
| num_literal {}
| num_literal PERCENT {}
| num_literal EURO {}
| INT_LITERAL date_qualifier {}
| constructor option(constructor_payload) {}

compare_op:
| LESSER {}
| LESSER_EQUAL {}
| GREATER {}
| GREATER_EQUAL {}
| EQUAL {}
| NOT_EQUAL {}

func:
| CARDINAL {}
| qident {}

aggregate_func:
| SUM {}
| CARDINAL {}

aggregate:
| aggregate_func FOR ident IN primitive_expression OF base_expression {}

base_expression:
| primitive_expression {}
| qident IN qident {}
| literal {}
| aggregate {}
| func OF separated_nonempty_list(COMMA, primitive_expression) {}
| qident WITH constructor {}

mult_op:
| MULT {}
| DIV {}

mult_expression:
| base_expression {}
| base_expression mult_op mult_expression {}

sum_op:
| PLUS {}
| MINUS {}

sum_expression:
| mult_expression {}
| mult_expression sum_op sum_expression {}

logical_op:
| AND {}
| OR {}
| IFF {}

logical_unop:
| NOT {}

compare_expression:
| sum_expression {}
| sum_expression compare_op compare_expression {}

logical_expression:
| compare_expression {}
| logical_unop compare_expression {}
| compare_expression logical_op logical_expression {}

optional_binding:
| {}
| OF ident {}
| OF LPAREN constructor_binding RPAREN {}

constructor_binding:
| constructor optional_binding {}

match_arm:
| constructor_binding COLON logical_expression {}

match_arms:
| ALT match_arm match_arms {}
| {}

forall_prefix:
| FOR ALL separated_nonempty_list(COMMA,ident) IN separated_nonempty_list(COMMA,qident) WE_HAVE {}

exists_prefix:
| EXISTS ident IN qident SUCH THAT {}

expression:
| exists_prefix expression {}
| forall_prefix expression {}
| MATCH primitive_expression WITH match_arms {}
| IF expression THEN expression ELSE base_expression {}
| logical_expression {}

condition:
| IF expression {}

condition_consequence:
| condition THEN {}

rule_parameters:
| DEPENDS definition_parameters {}

rule:
| option(rule_parameters) option(condition_consequence) option(forall_prefix) base_expression FILLED {}

definition_parameters:
| OF separated_nonempty_list(COMMA, ident) {}

definition:
| option(forall_prefix) qident option(definition_parameters) option(condition_consequence) DEFINED_AS expression {}

variation_type:
| INCREASING {}
| DECREASING {}

assertion_base:
| logical_expression {}
| qident FIXED BY ident {}
| qident VARIES_WITH base_expression option(variation_type) {}

assertion:
| option(condition_consequence) assertion_base {}
| exists_prefix assertion {}
| forall_prefix assertion {}

application_field_item:
| RULE option(OPTIONAL) rule {}
| DEFINITION option(OPTIONAL) definition {}
| ASSERTION assertion {}
| field_decl_includes {}

ident:
| i = IDENT { (i, mk_position $sloc) }

condition_pos:
| CONDITION { mk_position $sloc }

struct_field_base:
| DATA i= ident CONTENT t = typ {
  (i, t)
}
| pos = condition_pos i = ident {
  (i, (Condition, pos))
}

struct_field_func:
| DEPENDS OF t = typ { t }

struct_field:
| name_and_typ = struct_field_base func_typ = option(struct_field_func) {
  let (name, typ) = name_and_typ in
  let (typ, typ_pos) = typ in
  ({
    struct_decl_field_name = name;
    struct_decl_field_typ = match func_typ with
    | None -> (Base typ, typ_pos)
    | Some (return_typ, return_pos) -> (Func  {
      arg_typ = (typ, typ_pos);
      return_typ = (return_typ, return_pos);
    }, mk_position $sloc) ;
  }, mk_position $sloc)
}

field_decl_item:
| CONTEXT ident STRUCT constructor {}

field_decl_include:
| constructor DOT ident EQUAL constructor DOT ident option(condition)  {}

field_decl_includes_context:
| CONTEXT nonempty_list(field_decl_include) {}

field_decl_includes:
| INCLUDES FIELD constructor option(field_decl_includes_context) {}

enum_decl_line_payload:
| CONTENT t = typ { let (t, t_pos) = t in (Base t, t_pos) }

enum_decl_line:
| ALT c = constructor t = option(enum_decl_line_payload) { ({
    enum_decl_case_name = c;
    enum_decl_case_typ = t;
  }, mk_position $sloc) }

constructor:
| c = CONSTRUCTOR { (c, mk_position $sloc) }

code_item:
| FIELD constructor COLON nonempty_list(application_field_item) {
  (FieldUse (), mk_position $sloc)
}
| DECLARATION STRUCT c = constructor COLON fields = list(struct_field) {
  (StructDecl {
    struct_decl_name = c;
    struct_decl_fields = fields;
  }, mk_position $sloc)
}
| DECLARATION FIELD constructor COLON nonempty_list(field_decl_item) list(field_decl_includes) {
  (FieldDecl (), mk_position $sloc)
}
| DECLARATION ENUM c = constructor COLON cases = nonempty_list(enum_decl_line) {
  (EnumDecl {
    enum_decl_name = c;
    enum_decl_cases = cases;
  }, mk_position $sloc)
}

code:
| code = list(code_item) { (code, mk_position $sloc) }

metadata_block:
| BEGIN_CODE code_and_pos = code text = END_CODE END_METADATA {
  let (code, pos) = code_and_pos in
  (code, (text, pos))
}

source_file_item:
| title = LAW_ARTICLE { LawArticle title }
| code = LAW_CODE { LawCode code }
| text = LAW_TEXT { LawText text }
| BEGIN_METADATA code = metadata_block {
  let (code, source_repr) = code in
  MetadataBlock (code, source_repr)
}
| BEGIN_CODE code_and_pos = code text = END_CODE {
  let (code, pos) = code_and_pos in
  CodeBlock (code, (text, pos))
}

source_file:
| i = source_file_item f = source_file { i::f }
| EOF { [] }
