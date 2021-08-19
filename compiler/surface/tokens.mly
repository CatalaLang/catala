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
%token<string * string option * string option * int> LAW_HEADING

%token BEGIN_DIRECTIVE END_DIRECTIVE LAW_INCLUDE
%token<int> AT_PAGE
%token<string> DIRECTIVE_ARG

%token<string> LAW_TEXT
%token<string> CONSTRUCTOR IDENT
%token<string> END_CODE
%token<Runtime.integer> INT_LITERAL
%token TRUE FALSE
%token<Runtime.integer * Runtime.integer> DECIMAL_LITERAL
%token<Runtime.integer * Runtime.integer> MONEY_AMOUNT
%token BEGIN_CODE TEXT
%token COLON ALT DATA VERTICAL
%token OF INTEGER COLLECTION
%token RULE CONDITION DEFINED_AS
%token LESSER GREATER LESSER_EQUAL GREATER_EQUAL
%token LESSER_DEC GREATER_DEC LESSER_EQUAL_DEC GREATER_EQUAL_DEC
%token LESSER_MONEY GREATER_MONEY LESSER_EQUAL_MONEY GREATER_EQUAL_MONEY
%token LESSER_DATE GREATER_DATE LESSER_EQUAL_DATE GREATER_EQUAL_DATE
%token LESSER_DURATION GREATER_DURATION LESSER_EQUAL_DURATION GREATER_EQUAL_DURATION
%token EXISTS IN SUCH THAT
%token DOT AND OR XOR LPAREN RPAREN EQUAL
%token CARDINAL ASSERTION FIXED BY YEAR MONTH DAY
%token PLUS MINUS MULT DIV
%token PLUSDEC MINUSDEC MULTDEC DIVDEC
%token PLUSMONEY MINUSMONEY MULTMONEY DIVMONEY
%token MINUSDATE PLUSDATE PLUSDURATION MINUSDURATION DIVDURATION
%token CONCAT
%token MATCH WITH VARIES WITH_V WILDCARD
%token FOR ALL WE_HAVE INCREASING DECREASING
%token NOT BOOLEAN PERCENT DURATION
%token SCOPE FILLED NOT_EQUAL DEFINITION
%token STRUCT CONTENT IF THEN DEPENDS DECLARATION
%token CONTEXT ENUM ELSE DATE SUM
%token BEGIN_METADATA END_METADATA MONEY DECIMAL
%token UNDER_CONDITION CONSEQUENCE LBRACKET RBRACKET
%token LABEL EXCEPTION LSQUARE RSQUARE SEMICOLON
%token MAXIMUM MINIMUM INIT
%token FILTER MAP

%%
