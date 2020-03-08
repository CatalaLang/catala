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
%}

%token EOF
%token<string> LAW_ARTICLE
%token<string> LAW_CODE
%token<string> LAW_TEXT
%token<string> CONSTRUCTOR IDENT
%token<string> END_CODE
%token<int> INT_LITERAL
%token BEGIN_CODE CHOICE
%token COLON ALT DOT SITUATION SOURCE DATA
%token OF SEMICOLON INTEGER TYPE COLLECTION
%token RULE CONDITION CONSEQUENCE DEFINED AS
%token EXISTS IN SUCH THAT NOW LESSER GREATER
%token BANG AND OR LPAREN RPAREN OPTIONAL EQUAL
%token COMMA CARDINAL LESSER_EQUAL GREATER_EQUAL
%token ASSERTION FIXED BY CONSTANT YEAR
%token PLUS MINUS MULT DIV MATCH WITH VARIES_WITH
%token FORALL WE_HAVE INCREASING DECREASING
%token FUNCTION PARAMETERS RETURNS NOT

%type <Ast.source_file> source_file

%start source_file

%%

choice:
| CONSTRUCTOR situation_type {}

choices:
| ALT choice choices {}
| {}

type_ident:
| IDENT {}
| INTEGER {}

situation_type_alt:
| CHOICE IDENT {}
| TYPE type_ident {}
| SITUATION CONSTRUCTOR {}

situation_type:
| OF situation_type_alt {}
| {}

qident:
| IDENT {}
| IDENT BANG IDENT {}

primitive_expression:
| NOW {}

date_qualifier:
| YEAR {}

constructor_payload:
| OF base_expression {}

literal:
| INT_LITERAL {}
| INT_LITERAL date_qualifier {}
| CONSTRUCTOR option(constructor_payload) {}

compare_op:
| LESSER {}
| LESSER_EQUAL {}
| GREATER {}
| GREATER_EQUAL {}
| EQUAL {}

func:
| CARDINAL {}
| IDENT {}

base_expression:
| primitive_expression {}
| literal {}
| func LPAREN separated_nonempty_list(COMMA, expression) RPAREN {}
| qident {}
| LPAREN expression RPAREN {}

mult_op:
| MULT {}
| DIV {}

mult_expression:
| base_expression {}
| base_expression mult_op base_expression {}

sum_op:
| PLUS {}
| MINUS {}

sum_expression:
| mult_expression {}
| mult_expression sum_op mult_expression {}

logical_op:
| AND {}
| OR {}

logical_unop:
| NOT {}

compare_expression:
| sum_expression {}
| sum_expression compare_op sum_expression {}

logical_expression:
| compare_expression {}
| logical_unop compare_expression {}
| compare_expression logical_op compare_expression {}

optional_binding:
| {}
| OF IDENT {}
| OF LPAREN constructor_binding RPAREN {}

constructor_binding:
| CONSTRUCTOR optional_binding {}

match_arm:
| constructor_binding COLON logical_expression {}

match_arms:
| ALT match_arm match_arms {}
| {}

forall_prefix:
| FORALL separated_nonempty_list(COMMA,IDENT) IN separated_nonempty_list(COMMA,qident) WE_HAVE {}

exists_prefix:
| EXISTS IDENT IN qident SUCH THAT {}

expression:
| exists_prefix expression {}
| forall_prefix expression {}
| MATCH expression WITH  match_arms {}
| logical_expression {}

condition:
| CONDITION expression CONSEQUENCE {}


rule_definition_single:
| AS expression {}
| {}

rule_definition_various:
| AS logical_expression {}
| {}

rule_action_single:
| qident DEFINED rule_definition_single {}

rule_action_various:
| qident DEFINED rule_definition_various {}

rule_actions_various:
| ALT rule_action_various rule_actions_various {}
| {}

rule_actions:
| rule_action_single {}
| rule_actions_various {}

rule:
| option(condition) option(forall_prefix) rule_actions {}

variation_type:
| INCREASING {}
| DECREASING {}

assertion:
| logical_expression {}
| qident FIXED BY IDENT {}
| qident VARIES_WITH qident option(variation_type) {}
| exists_prefix assertion {}
| forall_prefix assertion {}

constant:
| IDENT situation_type DEFINED AS literal {}

func_parameter:
| IDENT situation_type {}

func_parameters:
| ALT func_parameter func_parameters {}
| {}

func_def:
| IDENT PARAMETERS func_parameters RETURNS type_ident COLON expression {}

situation:
| DATA IDENT option(COLLECTION) situation_type {}
| RULE option(OPTIONAL) rule {}
| ASSERTION option(condition) assertion {}
| CONSTANT constant {}
| FUNCTION func_def {}

code_item:
| CHOICE IDENT COLON choices { }
| SITUATION CONSTRUCTOR SOURCE IDENT COLON separated_nonempty_list(SEMICOLON, situation) { }

code:
| code_item DOT code {}
| {}

source_file_item:
| title = LAW_ARTICLE { LawArticle title }
| code = LAW_CODE { LawCode code }
| text = LAW_TEXT { LawText text }
| BEGIN_CODE code text = END_CODE { CodeBlock text }

source_file:
| i = source_file_item f = source_file { i::f }
| EOF { [] }
