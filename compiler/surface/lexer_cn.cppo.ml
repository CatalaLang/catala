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

(* Defining the lexer macros for Chinese *)

(* Tokens and their corresponding sedlex regexps *)

#define MS_SCOPE "\xe8\xaf\xad\xe5\xa2\x83"  
#define MR_SCOPE 0x8bed, 0x5883

#define MS_CONSEQUENCE "\xe7\xbb\x93\xe6\x9e\x9c" 
#define MR_CONSEQUENCE 0x7ed3, 0x679c

#define MS_DATA "\xe6\x95\xb0\xe6\x8d\xae"  
#define MR_DATA 0x6570, 0x636e

#define MS_DEPENDS "\xe5\x8f\x96\xe5\x86\xb3\xe4\xba\x8e"
#define MR_DEPENDS 0x53d6, 0x51b3, 0x4e8e

#define MS_DECLARATION "\xe5\xa3\xb0\xe6\x98\x8e"
#define MR_DECLARATION 0x58f0, 0x660e

#define MS_CONTEXT "\xe4\xb8\x8a\xe4\xb8\x8b\xe6\x96\x87"
#define MR_CONTEXT 0x4e0a, 0x4e0b, 0x6587

#define MS_DECREASING "\xe4\xb8\x8b\xe9\x99\x8d"
#define MR_DECREASING 0x4e0b, 0x964d

#define MS_INCREASING "\xe4\xb8\x8a\xe5\x8d\x87"
#define MR_INCREASING 0x4e0a, 0x5347

#define MS_OF "\xe7\x94\xa8\xe4\xba\x8e"
#define MR_OF 0x7528, 0x4e8e

#define MS_COLLECTION "\xe9\x9b\x86\xe5\x90\x88"
#define MR_COLLECTION 0x96c6, 0x5408

#define MS_CONTAINS "\xe5\x90\xab\xe6\x9c\x89"
#define MR_CONTAINS 0x542b, 0x6709

#define MS_ENUM "\xe6\x9e\x9a\xe4\xb8\xbe"
#define MR_ENUM 0x679a, 0x4e3e

#define MS_INTEGER "\xe6\x95\xb4\xe6\x95\xb0"
#define MR_INTEGER 0x6574, 0x6570

#define MS_MONEY "\xe8\xb4\xa7\xe5\xb8\x81"
#define MR_MONEY 0x8d27, 0x5e01

#define MS_TEXT "\xe6\x96\x87\xe6\x9c\xac"
#define MR_TEXT 0x6587, 0x672c

#define MS_DECIMAL "\xe5\xb0\x8f\xe6\x95\xb0"
#define MR_DECIMAL 0x5c0f, 0x6570

#define MS_DATE "\xe6\x97\xa5\xe6\x9c\x9f"
#define MR_DATE 0x65e5, 0x671f

#define MS_DURATION "\xe6\x97\xb6\xe9\x95\xbf"
#define MR_DURATION 0x65f6, 0x957f

#define MS_BOOLEAN "\xe7\x9c\x9f\xe5\x80\xbc"
#define MR_BOOLEAN 0x771f, 0x503c

#define MS_SUM "\xe5\x92\x8c\xe5\x80\xbc"
#define MR_SUM 0x548c, 0x503c

#define MS_FILLED "\xe6\xbb\xa1\xe8\xb6\xb3"
#define MR_FILLED 0x6ee1, 0x8db3

#define MS_DEFINITION "\xe5\xae\x9a\xe4\xb9\x89"
#define MR_DEFINITION 0x5b9a, 0x4e49

#define MS_STATE "\xe7\x8a\xb6\xe6\x80\x81"
#define MR_STATE 0x72b6, 0x6001

#define MS_LABEL "\xe6\xa0\x87\xe8\xae\xb0"
#define MR_LABEL 0x6807, 0x8bb0

#define MS_EXCEPTION "\xe4\xbe\x8b\xe5\xa4\x96"
#define MR_EXCEPTION 0x4f8b, 0x5916

#define MS_DEFINED_AS "\xe7\xad\x89\xe4\xba\x8e"
#define MR_DEFINED_AS 0x7b49, 0x4e8e

#define MS_MATCH "\xe5\x8c\xb9\xe9\x85\x8d"
#define MR_MATCH 0x5339, 0x914d

#define MS_WILDCARD "\xe4\xbb\xbb\xe4\xbd\x95"
#define MR_WILDCARD 0x4efb, 0x4f55

#define MS_WITH "\xe5\x92\x8c\xe6\xa8\xa1\xe5\xbc\x8f"
#define MR_WITH 0x548c, 0x6a21, 0x5f0f

#define MS_UNDER_CONDITION "\xe5\x9c\xa8\xe6\x9d\xa1\xe4\xbb\xb6\xe4\xb8\x8b"
#define MR_UNDER_CONDITION 0x5728, 0x6761, 0x4ef6, 0x4e0b

#define MS_IF "\xe5\xa6\x82\xe6\x9e\x9c"
#define MR_IF 0x5982, 0x679c

#define MS_THEN "\xe9\x82\xa3\xe4\xb9\x88"
#define MR_THEN 0x90a3, 0x4e48

#define MS_ELSE "\xe5\x90\xa6\xe5\x88\x99"
#define MR_ELSE 0x5426, 0x5219

#define MS_CONDITION "\xe6\x9d\xa1\xe4\xbb\xb6"
#define MR_CONDITION 0x6761, 0x4ef6

#define MS_CONTENT "\xe5\x86\x85\xe5\xae\xb9"
#define MR_CONTENT 0x5185, 0x5bb9

#define MS_STRUCT "\xe7\xbb\x93\xe6\x9e\x84"
#define MR_STRUCT 0x7ed3, 0x6784

#define MS_ASSERTION "\xe5\xa3\xb0\xe7\xa7\xb0"
#define MR_ASSERTION 0x58f0, 0x79f0

#define MS_VARIES "\xe5\x8f\x98\xe5\x8a\xa8"
#define MR_VARIES 0x53d8, 0x52a8

#define MS_WITH_V "\xe8\xbf\x9e\xe5\x90\x8c"
#define MR_WITH_V 0x8fde, 0x540c

#define MS_FOR "\xe5\xaf\xb9\xe4\xba\x8e"
#define MR_FOR 0x5bf9, 0x4e8e

#define MS_ALL "\xe6\x89\x80\xe6\x9c\x89"
#define MR_ALL 0x6240, 0x6709

#define MS_WE_HAVE "\xe6\x88\x91\xe4\xbb\xac\xe6\x9c\x89"
#define MR_WE_HAVE 0x6211, 0x4eec, 0x6709

#define MS_FIXED "\xe5\x9b\xba\xe5\xae\x9a"
#define MR_FIXED 0x56fa, 0x5b9a

#define MS_BY "\xe8\xa2\xab"
#define MR_BY 0x88ab

#define MS_RULE "\xe8\xa7\x84\xe5\xae\x9a"
#define MR_RULE 0x89c4, 0x5b9a

#define MS_LET "\xe8\xae\xa9"
#define MR_LET 0x8ba9

#define MS_EXISTS "\xe5\xad\x98\xe5\x9c\xa8"
#define MR_EXISTS 0x5b58, 0x5728

#define MS_IN "\xe7\x84\xb6\xe5\x90\x8e"
#define MR_IN 0x7136, 0x540e

#define MS_SUCH "\xe5\xa6\x82\xe6\xad\xa4"
#define MR_SUCH 0x5982, 0x6b64

#define MS_THAT "\xe4\xbb\x8e\xe8\x80\x8c"
#define MR_THAT 0x4ece, 0x800c

#define MS_INPUT "\xe8\xbe\x93\xe5\x85\xa5"
#define MR_INPUT 0x8f93, 0x5165

#define MS_OUTPUT "\xe8\xbe\x93\xe5\x87\xba"
#define MR_OUTPUT 0x8f93, 0x51fa

#define MS_INTERNAL "\xe5\x86\x85\xe9\x83\xa8"
#define MR_INTERNAL 0x5185, 0x90e8

#define MS_AND "and"
#define MS_OR "or"
#define MS_XOR "xor"
#define MS_NOT "not"
#define MS_MAXIMUM "maximum"
#define MS_MINIMUM "minimum"
#define MS_FILTER "filter"
#define MS_MAP "map"
#define MS_INIT "initial"
#define MS_CARDINAL "number"
#define MS_YEAR "year"
#define MS_MONTH "month"
#define MS_DAY "day"
#define MS_TRUE "true"
#define MS_FALSE "false"


(* Specific delimiters *)

#define MR_MONEY_OP_SUFFIX 0x00A5 (* the RMB sign *)
#define MC_DECIMAL_SEPARATOR '.'
#define MR_MONEY_PREFIX 0x00A5, Star hspace
#define MR_MONEY_DELIM ','
#define MR_MONEY_SUFFIX ""

(* Builtins *)

#define MS_RoundMoney "round_money"
#define MS_RoundDecimal "round_decimal"
#define MS_IntToDec "integer_to_decimal"
#define MS_MoneyToDec "money_to_decimal"
#define MS_DecToMoney "decimal_to_money"
#define MS_GetDay "get_day"
#define MS_GetMonth "get_month"
#define MS_GetYear "get_year"
#define MS_FirstDayOfMonth "first_day_of_month"
#define MS_LastDayOfMonth "last_day_of_month"

(* Directives *)

#define MS_LAW_INCLUDE "\xe5\xbc\x95\xe7\x94\xa8"
#define MR_LAW_INCLUDE 0x5f15, 0x7528

#define MX_AT_PAGE \
   '@', Star hspace, "p.", Star hspace, Plus digit -> \
      let s = Utf8.lexeme lexbuf in \
      let i = String.index s '.' in \
      AT_PAGE (int_of_string (String.trim (String.sub s i (String.length s - i))))
