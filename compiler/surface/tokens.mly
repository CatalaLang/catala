(*
  This file is part of the Catala compiler, a specification language for tax and social benefits
  computation rules.
  Copyright (C) 2020 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr>,
  Emile Rolley <emile.rolley@tuta.io>

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
%token<string * string option * bool * int> LAW_HEADING

%token BEGIN_DIRECTIVE END_DIRECTIVE LAW_INCLUDE MODULE_DEF MODULE_USE MODULE_ALIAS
%token<int> AT_PAGE
%token<string> DIRECTIVE_ARG

%token<string> LAW_TEXT
%token<string> UIDENT LIDENT
%token<string> END_CODE
%token<string> INT_LITERAL
%token<int * int * int> DATE_LITERAL
%token TRUE FALSE
%token<string * string> DECIMAL_LITERAL
%token<string * string> MONEY_AMOUNT
%token BEGIN_CODE TEXT
%token COLON ALT DATA
%token OF INTEGER COLLECTION CONTAINS AMONG
%token RULE CONDITION DEFINED_AS
%token<Ast.op_kind> LESSER GREATER LESSER_EQUAL GREATER_EQUAL
%token LET EXISTS IN SUCH THAT COMMA
%token DOT AND OR XOR LPAREN RPAREN EQUAL
%token CARDINAL ASSERTION FIXED BY YEAR MONTH DAY
%token<Ast.op_kind> PLUS MINUS MULT DIV
%token PLUSPLUS
%token MATCH WITH VARIES WITH_V WILDCARD
%token FOR ALL WE_HAVE INCREASING DECREASING
%token NOT BOOLEAN PERCENT DURATION
%token SCOPE FILLED NOT_EQUAL DEFINITION STATE
%token STRUCT CONTENT IF THEN DEPENDS DECLARATION
%token CONTEXT INPUT OUTPUT INTERNAL ENUM ELSE DATE SUM
%token BEGIN_METADATA MONEY DECIMAL
%token UNDER_CONDITION CONSEQUENCE LBRACE RBRACE
%token LABEL EXCEPTION LBRACKET RBRACKET SEMICOLON
%token MAXIMUM MINIMUM IS EMPTY

%%
