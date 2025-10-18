(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Jan Cavel <caveljan@piatra.institute>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(* Defining the lexer macros for Romanian *)

(* Tokens and their corresponding sedlex regexps *)

#define MS_SCOPE "scop"
#define MS_CONSEQUENCE "consecință"
#define MR_CONSEQUENCE "consecin", 0x021B, 0x0103
#define MS_DATA "date"
#define MS_DEPENDS "depinde de"
#define MR_DEPENDS "depinde", space_plus, "de"
#define MS_DECLARATION "declarație"
#define MR_DECLARATION "declara", 0x021B, "ie"
#define MS_CONTEXT "context"
#define MS_DECREASING "descrescător"
#define MR_DECREASING "descresc", 0x0103, "tor"
#define MS_INCREASING "crescător"
#define MR_INCREASING "cresc", 0x0103, "tor"
#define MS_OF "de"
#define MS_LIST "listă de"
#define MR_LIST "list", 0x0103, space_plus, "de"
#define MS_OPTION "opțional de"
#define MR_OPTION "op", 0x021B, "ional", space_plus, "de"
#define MS_CONTAINS "conține"
#define MR_CONTAINS "con", 0x021B, "ine"
#define MS_ENUM "enumerare"
#define MS_SUM "sumă"
#define MR_SUM "sum", 0x0103
#define MS_FILLED "îndeplinit"
#define MR_FILLED 0x00EE, "ndeplinit"
#define MS_DEFINITION "definiție"
#define MR_DEFINITION "defini", 0x021B, "ie"
#define MS_STATE "stare"
#define MS_LABEL "etichetă"
#define MR_LABEL "etichet", 0x0103
#define MS_EXCEPTION "excepție"
#define MR_EXCEPTION "excep", 0x021B, "ie"
#define MS_DEFINED_AS "egal"
#define MS_MATCH "potrivește"
#define MR_MATCH "potrive", 0x0219, "te"
#define MS_WILDCARD "orice"
#define MS_TYPE "tip"
#define MS_WITH "cu model"
#define MR_WITH "cu", space_plus, "model"
#define MS_UNDER_CONDITION "în condiția"
#define MR_UNDER_CONDITION 0x00EE, "n", space_plus, "condi", 0x021B, "ia"
#define MS_IF "dacă"
#define MR_IF "dac", 0x0103
#define MS_THEN "atunci"
#define MS_ELSE "altfel"
#define MS_CONDITION "condiție"
#define MR_CONDITION "condi", 0x021B, "ie"
#define MS_CONTENT "conținut"
#define MR_CONTENT "con", 0x021B, "inut"
#define MS_STRUCT "structură"
#define MR_STRUCT "structur", 0x0103
#define MS_ASSERTION "aserțiune"
#define MR_ASSERTION "aser", 0x021B, "iune"
#define MS_WITH_V "cu"
#define MS_FOR "pentru"
#define MS_ALL "toți"
#define MR_ALL "to", 0x021B, "i"
#define MS_WE_HAVE "avem"
#define MS_RULE "regulă"
#define MR_RULE "regul", 0x0103
#define MS_LET "fie"
#define MS_EXISTS "există"
#define MR_EXISTS "exist", 0x0103
#define MS_IN "în"
#define MR_IN 0x00EE, "n"
#define MS_AMONG "printre"
#define MS_COMBINE "combină"
#define MR_COMBINE "combin", 0x0103
#define MS_MAP_EACH "aplică fiecare"
#define MR_MAP_EACH "aplic", 0x0103, space_plus, "fiecare"
#define MS_TO "la"
#define MS_SUCH "astfel"
#define MS_THAT "că"
#define MR_THAT "c", 0x0103
#define MS_AND "și"
#define MR_AND 0x0219, "i"
#define MS_OR "sau"
#define MS_XOR "sau exclusiv"
#define MR_XOR "sau", space_plus, "exclusiv"
#define MS_NOT "nu"
#define MS_MAXIMUM "maxim"
#define MS_MINIMUM "minim"
#define MS_IS "este"
#define MS_OR_IF_LIST_EMPTY "sau dacă lista este goală"
#define MR_OR_IF_LIST_EMPTY "sau", space_plus, "dac", 0x0103, space_plus, "lista", space_plus, "este", space_plus, "goal", 0x0103
#define MS_BUT_REPLACE "dar înlocuiește"
#define MR_BUT_REPLACE "dar", space_plus, 0x00EE, "nlocuie", 0x0219, "te"
#define MS_INITIALLY "inițial"
#define MR_INITIALLY "ini", 0x021B, "ial"
#define MS_YEAR "an"
#define MS_MONTH "lună"
#define MR_MONTH "lun", 0x0103
#define MS_DAY "zi"
#define MS_TRUE "adevărat"
#define MR_TRUE "adev", 0x0103, "rat"
#define MS_FALSE "fals"
#define MS_INPUT "intrare"
#define MS_OUTPUT "ieșire"
#define MR_OUTPUT "ie", 0x0219, "ire"
#define MS_INTERNAL "intern"

(* Specific delimiters *)

#define MS_MONEY_OP_SUFFIX "RON"
#define MC_DECIMAL_SEPARATOR ','
#define MR_MONEY_PREFIX ""
#define MR_MONEY_DELIM '.'
#define MR_MONEY_SUFFIX Star hspace, "RON"

(* Builtin types *)

#define MS_INTEGER "întreg"
#define MR_INTEGER 0x00EE, "ntreg"
#define MS_MONEY "bani"
#define MS_DECIMAL "zecimal"
#define MS_DATE "dată"
#define MR_DATE "dat", 0x0103
#define MS_DURATION "durată"
#define MR_DURATION "durat", 0x0103
#define MS_BOOLEAN "boolean"
#define MS_POSITION "position_source"

(* Builtin constructors *)

#define MS_PRESENT "Prezent"
#define MS_ABSENT "Absent"

(* Builtin functions *)

#define MS_Round "rotunjește"
#define MR_Round "rotunje", 0x0219, "te"
#define MS_Impossible "imposibil"
#define MS_Cardinal "număr"
#define MR_Cardinal "num", 0x0103, "r"

(* Directives *)

#define MR_LAW_INCLUDE "Include"
#define MR_MODULE_DEF "Modul"
#define MR_MODULE_USE "Folosind"
#define MR_MODULE_ALIAS "ca"
#define MR_EXTERNAL "extern"
#define MX_AT_PAGE \
   '@', Star hspace, "p.", Star hspace, Plus digit -> \
      let s = Utf8.lexeme lexbuf in \
      let i = String.index s '.' in \
      AT_PAGE (int_of_string (String.trim (String.sub s i (String.length s - i))))
