(* -*- coding: iso-latin-1 -*- *)

(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr>, Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(* Defining the lexer macros for English *)

(* Tokens and their corresponding sedlex regexps *)

#define MS_SCOPE "scope"
#define MS_CONSEQUENCE "consequence"
#define MS_DATA "data"
#define MS_DEPENDS "depends on"
#define MR_DEPENDS "depends", space_plus, "on"
#define MS_DECLARATION "declaration"
#define MS_CONTEXT "context"
#define MS_DECREASING "decreasing"
#define MS_INCREASING "increasing"
#define MS_OF "of"
#define MS_COLLECTION "collection"
#define MS_CONTAINS "contains"
#define MS_ENUM "enumeration"
#define MS_INTEGER "integer"
#define MS_MONEY "money"
#define MS_TEXT "text"
#define MS_DECIMAL "decimal"
#define MS_DATE "date"
#define MS_DURATION "duration"
#define MS_BOOLEAN "boolean"
#define MS_SUM "sum"
#define MS_FILLED "fulfilled"
#define MS_DEFINITION "definition"
#define MS_STATE "state"
#define MS_LABEL "label"
#define MS_EXCEPTION "exception"
#define MS_DEFINED_AS "equals"
#define MS_MATCH "match"
#define MS_WILDCARD "anything"
#define MS_WITH "with pattern"
#define MR_WITH "with", space_plus, "pattern"
#define MS_UNDER_CONDITION "under condition"
#define MR_UNDER_CONDITION "under", space_plus, "condition"
#define MS_IF "if"
#define MS_THEN "then"
#define MS_ELSE "else"
#define MS_CONDITION "condition"
#define MS_CONTENT "content"
#define MS_STRUCT "structure"
#define MS_ASSERTION "assertion"
#define MS_VARIES "varies"
#define MS_WITH_V "with"
#define MS_FOR "for"
#define MS_ALL "all"
#define MS_WE_HAVE "we have"
#define MR_WE_HAVE "we", space_plus, "have"
#define MS_FIXED "fixed"
#define MS_BY "by"
#define MS_RULE "rule"
#define MS_LET "let"
#define MS_EXISTS "exists"
#define MS_IN "in"
#define MS_AMONG "among"
#define MS_SUCH "such"
#define MS_THAT "that"
#define MS_AND "and"
#define MS_OR "or"
#define MS_XOR "xor"
#define MS_NOT "not"
#define MS_MAXIMUM "maximum"
#define MS_MINIMUM "minimum"
#define MS_IS "is"
#define MS_EMPTY "empty"
#define MS_CARDINAL "number"
#define MS_YEAR "year"
#define MS_MONTH "month"
#define MS_DAY "day"
#define MS_TRUE "true"
#define MS_FALSE "false"
#define MS_INPUT "input"
#define MS_OUTPUT "output"
#define MS_INTERNAL "internal"

(* Specific delimiters *)

#define MS_MONEY_OP_SUFFIX "$"
#define MC_DECIMAL_SEPARATOR '.'
#define MR_MONEY_PREFIX '$', Star hspace
#define MR_MONEY_DELIM ','
#define MR_MONEY_SUFFIX ""

(* Builtins *)

#define MS_Round "round"
#define MS_GetDay "get_day"
#define MS_GetMonth "get_month"
#define MS_GetYear "get_year"
#define MS_FirstDayOfMonth "first_day_of_month"
#define MS_LastDayOfMonth "last_day_of_month"

(* Directives *)

#define MR_LAW_INCLUDE "Include"
#define MR_MODULE_DEF "Module"
#define MR_MODULE_USE "Using"
#define MR_MODULE_ALIAS "as"
#define MR_EXTERNAL "external"
#define MX_AT_PAGE \
   '@', Star hspace, "p.", Star hspace, Plus digit -> \
      let s = Utf8.lexeme lexbuf in \
      let i = String.index s '.' in \
      AT_PAGE (int_of_string (String.trim (String.sub s i (String.length s - i))))
