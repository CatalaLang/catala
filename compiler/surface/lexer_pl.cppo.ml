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

(* Defining the lexer macros for Polish *)

(* Tokens and their corresponding sedlex regexps *)

#define MS_SCOPE "zakres"
#define MS_CONSEQUENCE "konsekwencja"
#define MS_DATA "dane"
#define MS_DEPENDS "zależy od"
#define MR_DEPENDS "zale", 0x017C, "y", space_plus, "od"
#define MS_DECLARATION "deklaracja"
#define MS_CONTEXT "kontekst"
#define MS_DECREASING "dół"
#define MR_DECREASING "d", 0xf3, 0x0142
#define MS_INCREASING "górę"
#define MR_INCREASING "g", 0xf3, "r", 0x0119
#define MS_OF "z"
#define MS_LIST "lista"
#define MS_OPTION "opcjonalny"
#define MS_CONTAINS "zawiera"
#define MS_ENUM "enumeracja"
#define MS_SUM "suma"
#define MS_FILLED "spełnione"
#define MR_FILLED "spe", 0x0142, "nione"
#define MS_DEFINITION "definicja"
#define MS_STATE "stan"
#define MS_LABEL "etykieta"
#define MS_EXCEPTION "wyjątek"
#define MR_EXCEPTION "wyj", 0x0105, "tek"
#define MS_DEFINED_AS "wynosi"
#define MS_MATCH "pasuje"
#define MS_WILDCARD "cokolwiek"
#define MS_TYPE "rodzaj"
#define MS_WITH "ze wzorem"
#define MR_WITH "ze", space_plus, "wzorem"
#define MS_UNDER_CONDITION "pod warunkiem"
#define MR_UNDER_CONDITION "pod", space_plus, "warunkiem"
#define MS_IF "jeżeli"
#define MR_IF "je", 0x017C, "eli"
#define MS_THEN "wtedy"
#define MS_ELSE "inaczej"
#define MS_CONDITION "warunek"
#define MS_CONTENT "typu"
#define MS_STRUCT "struktura"
#define MS_ASSERTION "asercja"
#define MS_WITH_V "wraz z"
#define MR_WITH_V "wraz", space_plus, "z"
#define MS_FOR "dla"
#define MS_ALL "wszystkie"
#define MS_WE_HAVE "mamy"
#define MS_RULE "zasada"
#define MS_LET "niech"
#define MS_EXISTS "istnieje"
#define MS_IN "w"
#define MS_AMONG "wśród"
#define MR_AMONG "w", 0x15B, "r", 0xf3, "d"
#define MS_COMBINE "połączyć"
#define MR_COMBINE "po", 0x0142, 0x0105 , "czy", 0x0107
#define MS_MAP_EACH "plan każdy"
#define MR_MAP_EACH "plan", space_plus, "ka", 0x017C, "dy"
#define MS_TO "do"
#define MS_SUCH "takie że"
#define MR_SUCH "takie", space_plus, 0x017C, "e"
#define MS_THAT "to"
#define MS_AND "i"
#define MS_OR "lub"
#define MS_XOR "albo"
#define MS_NOT "nie"
#define MS_MAXIMUM "maksimum"
#define MS_MINIMUM "minimum"
#define MS_IS "jest"
#define MS_OR_IF_LIST_EMPTY "lub jeżeli lista pusta"
#define MR_OR_IF_LIST_EMPTY "lub", space_plus, "je", 0x017C, "eli", space_plus, "lista", space_plus, "pusta"
#define MS_BUT_REPLACE "ale zastąpić"
#define MR_BUT_REPLACE "ale", space_plus, "zast", 0x0105, "pi", 0x0107
#define MS_INITIALLY "początkowo"
#define MR_INITIALLY "pocz", 0X0105, "tkowo"
#define MS_YEAR "rok"
#define MS_MONTH "miesiąc"
#define MR_MONTH "miesi", 0x0105, "c"
#define MS_DAY "dzień"
#define MR_DAY "dzie", 0x0144
#define MS_TRUE "prawda"
#define MS_FALSE "fałsz"
#define MR_FALSE "fa", 0x0142, "sz"
#define MS_INPUT "wejście"
#define MR_INPUT "wej", 0x15B, "cie"
#define MS_OUTPUT "wyjście"
#define MR_OUTPUT "wyj", 0x15B, "cie"
#define MS_INTERNAL "wewnętrzny"
#define MR_INTERNAL "wewn", 0x0119, "trzny"

(* Specific delimiters *)

#define MS_MONEY_OP_SUFFIX "PLN"
#define MC_DECIMAL_SEPARATOR '.'
#define MR_MONEY_PREFIX ""
#define MR_MONEY_DELIM ','
#define MR_MONEY_SUFFIX Star hspace, "PLN"

(* Builtin types *)

#define MS_INTEGER "całkowita"
#define MR_INTEGER "ca", 0x0142, "kowita"
#define MS_MONEY "pieniądze"
#define MR_MONEY "pieni", 0x0105, "dze"
#define MS_DECIMAL "dziesiętny"
#define MR_DECIMAL "dziesi", 0x0119, "tny"
#define MS_DATE "czas"
#define MS_DURATION "czastrwania"
#define MS_BOOLEAN "zerojedynkowy"
#define MS_POSITION "code_location"

(* Builtin constructors *)

#define MS_PRESENT "Obecny"
#define MS_ABSENT "Nieobecny"

(* Builtin functions *)

#define MS_Round "zaokrąglony"
#define MR_Round "zaokr", 0x0105, "glony"
#define MS_Impossible "niemożliwe"
#define MR_Impossible "niemo", 0x017C, "liwe"
#define MS_Cardinal "liczba"


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
