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

(* Defining the lexer macros for Polish *)

(* Tokens and their corresponding sedlex regexps *)

#define MS_SCOPE "zakres"
#define MS_CONSEQUENCE "konsekwencja"
#define MS_DATA "data"
#define MS_DEPENDS "zalezy od"
#define MR_DEPENDS "zalezy", space_plus, "od"
#define MS_DECLARATION "deklaracja"
#define MS_CONTEXT "kontekst"
#define MS_DECREASING "malejacy"
#define MS_INCREASING "rosnacy"
#define MS_OF "z"
#define MS_COLLECTION "kolekcja"
#define MS_ENUM "enumeracja"
#define MS_INTEGER "calkowita"
#define MS_MONEY "pieni\x01\x05dze"
#define MR_MONEY "pieni", 0x0105, "dze"
#define MS_TEXT "tekst"
#define MS_DECIMAL "dziesi\x01\x19tny"
#define MR_DECIMAL "dziesi", 0x0119, "tny"
#define MS_DATE "czas"
#define MS_DURATION "czas trwania"
#define MR_DURATION "czas", space_plus, "trwania"
#define MS_BOOLEAN "zerojedynkowy"
#define MS_SUM "suma"
#define MS_FILLED "spelnione"
#define MS_DEFINITION "definicja"
#define MS_LABEL "etykieta"
#define MS_EXCEPTION "wyj\x01\x05tek"
#define MR_EXCEPTION "wyj", 0x0105, "tek"
#define MS_DEFINED_AS "wynosi"
#define MS_MATCH "pasuje"
#define MS_WILDCARD "cokolwiek"
#define MS_WITH "ze wzorem"
#define MR_WITH "ze", space_plus, "wzorem"
#define MS_UNDER_CONDITION "pod warunkiem"
#define MR_UNDER_CONDITION "pod", space_plus, "warunkiem"
#define MS_IF "jezeli"
#define MS_THEN "wtedy"
#define MS_ELSE "inaczej"
#define MS_CONDITION "warunek"
#define MS_CONTENT "typu"
#define MS_STRUCT "struktura"
#define MS_ASSERTION "asercja"
#define MS_VARIES "rozna"
#define MS_WITH_V "wraz z"
#define MR_WITH_V "wraz", space_plus, "z"
#define MS_FOR "dla"
#define MS_ALL "wszystkie"
#define MS_WE_HAVE "mamy"
#define MS_FIXED "staloprzecinkowa"
#define MS_BY "przez"
#define MS_RULE "zasada"
#define MS_EXISTS "istnieje"
#define MS_IN "in"
#define MS_SUCH "takie ze"
#define MR_SUCH "takie", space_plus, "ze"
#define MS_THAT "to"
#define MS_AND "i"
#define MS_OR "lub"
#define MS_XOR "xor"
#define MS_NOT "nie"
#define MS_MAXIMUM "maximum"
#define MS_MINIMUM "minimum"
#define MS_FILTER "filtr"
#define MS_MAP "mapuj"
#define MS_INIT "poczatkowy"
#define MS_CARDINAL "liczba"
#define MS_YEAR "rok"
#define MS_MONTH "miesiac"
#define MS_DAY "dzien"
#define MS_TRUE "prawda"
#define MS_FALSE "falsz"

(* Specific delimiters *)

#define MR_MONEY_OP_SUFFIX '$'
#define MC_DECIMAL_SEPARATOR '.'
#define MR_MONEY_PREFIX ""
#define MR_MONEY_DELIM ','
#define MR_MONEY_SUFFIX Star hspace, "PLN"

(* Builtins *)

#define MS_IntToDec "integer_to_decimal"
#define MS_GetDay "get_day"
#define MS_GetMonth "get_month"
#define MS_GetYear "get_year"

(* Directives *)

#define MR_LAW_INCLUDE "Include"
#define MX_AT_PAGE \
   '@', Star hspace, "p.", Star hspace, Plus digit -> \
      let s = Utf8.lexeme lexbuf in \
      let i = String.index s '.' in \
      AT_PAGE (int_of_string (String.trim (String.sub s i (String.length s - i))))
