(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria,
   contributors: Vincent Botbol <vincent.botbol@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

let token_to_string =
  let open Tokens in
  let open Ast in
  let op_to_string = function
    | KPoly -> "POLY"
    | KInt -> "INT"
    | KDec -> "DEC"
    | KMoney -> "MONEY"
    | KDate -> "DATE"
    | KDuration -> "DURATION"
  in
  function
  | Tokens.YEAR -> "YEAR"
  | XOR -> "XOR"
  | WITH_V -> "WITH_V"
  | WITH -> "WITH"
  | WILDCARD -> "WILDCARD"
  | WE_HAVE -> "WE_HAVE"
  | UNDER_CONDITION -> "UNDER_CONDITION"
  | UIDENT s -> Format.sprintf "UIDENT(%s)" s
  | TYPE -> "TYPE"
  | TRUE -> "TRUE"
  | TO -> "TO"
  | THEN -> "THEN"
  | THAT -> "THAT"
  | SUM -> "SUM"
  | SUCH -> "SUCH"
  | STRUCT -> "STRUCT"
  | STRING s -> Format.sprintf "STRING(%s)" s
  | STATE -> "STATE"
  | SEMICOLON -> "SEMICOLON"
  | SCOPE -> "SCOPE"
  | RULE -> "RULE"
  | RPAREN -> "RPAREN"
  | RBRACKET -> "RBRACKET"
  | RBRACE -> "RBRACE"
  | PLUSPLUS -> "PLUSPLUS"
  | PLUS op -> Format.sprintf "PLUS(%s)" (op_to_string op)
  | MINUS op -> Format.sprintf "MINUS(%s)" (op_to_string op)
  | MULT op -> Format.sprintf "MULT(%s)" (op_to_string op)
  | DIV op -> Format.sprintf "DIV(%s)" (op_to_string op)
  | PERCENT -> "PERCENT"
  | OUTPUT -> "OUTPUT"
  | OR_IF_LIST_EMPTY -> "OR_IF_LIST_EMPTY"
  | OR -> "OR"
  | OPTION -> "OPTION"
  | OF -> "OF"
  | NOT_EQUAL -> "NOT_EQUAL"
  | NOT -> "NOT"
  | MONTH -> "MONTH"
  | MONEY_AMOUNT _ -> "MONEY_AMOUNT _"
  | MODULE_USE -> "MODULE_USE"
  | MODULE_DEF -> "MODULE_DEF"
  | MODULE_ALIAS -> "MODULE_ALIAS"
  | MINIMUM -> "MINIMUM"
  | MAXIMUM -> "MAXIMUM"
  | MATCH -> "MATCH"
  | MAP_EACH -> "MAP_EACH"
  | LPAREN -> "LPAREN"
  | LIST -> "LIST"
  | LIDENT _ -> "LIDENT _"
  | LET -> "LET"
  | LESSER_EQUAL op -> Format.sprintf "LESSER_EQUAL(%s)" (op_to_string op)
  | LESSER op -> Format.sprintf "LESSER(%s)" (op_to_string op)
  | GREATER_EQUAL op -> Format.sprintf "GREATER_EQUAL(%s)" (op_to_string op)
  | GREATER op -> Format.sprintf "GREATER(%s)" (op_to_string op)
  | LBRACKET -> "LBRACKET"
  | LBRACE -> "LBRACE"
  | LAW_TEXT _ -> "LAW_TEXT"
  | LAW_INCLUDE -> "LAW_INCLUDE"
  | LAW_HEADING _ -> "LAW_HEADING"
  | LABEL -> "LABEL"
  | IS -> "IS"
  | INT_LITERAL s -> Format.sprintf "INT_LITERAL(%s)" s
  | INTERNAL -> "INTERNAL"
  | INPUT -> "INPUT"
  | INITIALLY -> "INITIALLY"
  | INCREASING -> "INCREASING"
  | IN -> "IN"
  | IF -> "IF"
  | FOR -> "FOR"
  | FILLED -> "FILLED"
  | FALSE -> "FALSE"
  | EXTERNAL -> "EXTERNAL"
  | EXISTS -> "EXISTS"
  | EXCEPTION -> "EXCEPTION"
  | EQUAL -> "EQUAL"
  | EOF -> "EOF"
  | ENUM -> "ENUM"
  | END_DIRECTIVE -> "END_DIRECTIVE"
  | END_CODE s -> Format.sprintf "END_CODE(%s)" s
  | ELSE -> "ELSE"
  | DOT -> "DOT"
  | DOCSTRING _ -> "DOCSTRING"
  | DIRECTIVE_ARG s -> Format.sprintf "DIRECTIVE_ARG(%s)" s
  | DEPENDS -> "DEPENDS"
  | DEFINITION -> "DEFINITION"
  | DEFINED_AS -> "DEFINED_AS"
  | DECREASING -> "DECREASING"
  | DECLARATION -> "DECLARATION"
  | DECIMAL_LITERAL (l, r) -> Format.sprintf "DECIMAL_LITERAL(%s,%s)" l r
  | DAY -> "DAY"
  | DATE_LITERAL (y, m, d) -> Format.sprintf "DATE_LITERAL(%d,%d,%d)" y m d
  | DATE -> "DATE"
  | DATA -> "DATA"
  | CONTEXT -> "CONTEXT"
  | CONTENT -> "CONTENT"
  | CONTAINS -> "CONTAINS"
  | CONSEQUENCE -> "CONSEQUENCE"
  | CONDITION -> "CONDITION"
  | COMMA -> "COMMA"
  | COMBINE -> "COMBINE"
  | COLON -> "COLON"
  | BUT_REPLACE -> "BUT_REPLACE"
  | BEGIN_METADATA -> "BEGIN_METADATA"
  | BEGIN_DIRECTIVE -> "BEGIN_DIRECTIVE"
  | BEGIN_CODE -> "BEGIN_CODE"
  | AT_PAGE i -> Format.sprintf "AT_PAGE(%i)" i
  | ATTR_START -> "ATTR_START"
  | ASSERTION -> "ASSERTION"
  | AND -> "AND"
  | AMONG -> "AMONG"
  | ALT -> "ALT"
  | ALL -> "ALL"
