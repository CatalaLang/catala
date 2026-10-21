(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr>, Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing permissions and limitations under the
   License. *)

(* Defining the lexer macros for Spain Spanish. The source is UTF-8 encoded. *)

#define MS_SCOPE "ámbito"
#define MR_SCOPE 0xE1, "mbito"
#define MS_CONSEQUENCE "consecuencia"
#define MS_DATA "dato"
#define MS_DEPENDS "depende de"
#define MR_DEPENDS "depende", space_plus, "de"
#define MS_DECLARATION "declaración"
#define MR_DECLARATION "declaraci", 0xF3, "n"
#define MS_CONTEXT "contexto"
#define MS_DECREASING "decreciente"
#define MS_INCREASING "creciente"
#define MS_OF "de"
#define MS_LIST "lista de"
#define MR_LIST "lista", space_plus, "de"
#define MS_OPTION "opcional de"
#define MR_OPTION "opcional", space_plus, "de"
#define MS_CONTAINS "contiene"
#define MS_ENUM "enumeración"
#define MR_ENUM "enumeraci", 0xF3, "n"
#define MS_SUM "suma"
#define MS_FILLED "cumplida"
#define MS_DEFINITION "definición"
#define MR_DEFINITION "definici", 0xF3, "n"
#define MS_STATE "estado"
#define MS_LABEL "etiqueta"
#define MS_EXCEPTION "excepción"
#define MR_EXCEPTION "excepci", 0xF3, "n"
#define MS_DEFINED_AS "igual a"
#define MR_DEFINED_AS "igual", space_plus, "a"
#define MS_MATCH "coincide"
#define MS_WILDCARD "cualquiera"
#define MS_TYPE "tipo"
#define MS_WITH "con patrón"
#define MR_WITH "con", space_plus, "patr", 0xF3, "n"
#define MS_UNDER_CONDITION "bajo condición"
#define MR_UNDER_CONDITION "bajo", space_plus, "condici", 0xF3, "n"
#define MS_IF "si"
#define MS_THEN "entonces"
#define MS_ELSE "si no"
#define MR_ELSE "si", space_plus, "no"
#define MS_CONDITION "condición"
#define MR_CONDITION "condici", 0xF3, "n"
#define MS_CONTENT "contenido"
#define MS_STRUCT "estructura"
#define MS_ASSERTION "aserción"
#define MR_ASSERTION "aserci", 0xF3, "n"
#define MS_WITH_V "con"
#define MS_FOR "para"
#define MS_ALL "todos"
#define MS_WE_HAVE "tenemos"
#define MR_WE_HAVE "tenemos"
#define MS_RULE "regla"
#define MS_LET "sea"
#define MS_EXISTS "existe"
#define MS_IN "en"
#define MS_AMONG "entre"
#define MS_COMBINE "combinar"
#define MS_MAP_EACH "transformar cada"
#define MR_MAP_EACH "transformar", space_plus, "cada"
#define MS_TO "a"
#define MS_SUCH "tal"
#define MS_THAT "que"
#define MS_SORT "ordenar"
#define MS_ORDER_ASCENDING "en orden creciente"
#define MR_ORDER_ASCENDING "en", space_plus, "orden", space_plus, "creciente"
#define MS_ORDER_DESCENDING "en orden decreciente"
#define MR_ORDER_DESCENDING "en", space_plus, "orden", space_plus, "decreciente"
#define MS_AND_THEN "y después"
#define MR_AND_THEN "y", space_plus, "despu", 0xE9, "s"
#define MS_AND "y"
#define MS_OR "o"
#define MS_XOR "o bien"
#define MR_XOR "o", space_plus, "bien"
#define MS_NOT "no"
#define MS_MAXIMUM "máximo"
#define MR_MAXIMUM "m", 0xE1, "ximo"
#define MS_MINIMUM "mínimo"
#define MR_MINIMUM "m", 0xED, "nimo"
#define MS_IS "es"
#define MS_OR_IF_LIST_EMPTY "o si la lista está vacía"
#define MR_OR_IF_LIST_EMPTY "o", space_plus, "si", space_plus, "la", space_plus, "lista", space_plus, "est", 0xE1, space_plus, "vac", 0xED, "a"
#define MS_BUT_REPLACE "pero reemplazando"
#define MR_BUT_REPLACE "pero", space_plus, "reemplazando"
#define MS_INITIALLY "inicialmente"
#define MS_YEAR "año"
#define MR_YEAR "a", 0xF1, "o"
#define MS_MONTH "mes"
#define MS_DAY "día"
#define MR_DAY "d", 0xED, "a"
#define MS_TRUE "verdadero"
#define MS_FALSE "falso"
#define MS_INPUT "entrada"
#define MS_OUTPUT "salida"
#define MS_INTERNAL "interno"

#define MS_MONEY_OP_SUFFIX "€"
#define MR_MONEY_OP_SUFFIX 0x20AC
#define MC_DECIMAL_SEPARATOR ','
#define MR_MONEY_PREFIX ""
#define MR_MONEY_DELIM ' '
#define MR_MONEY_SUFFIX Star hspace, 0x20AC

#define MS_INTEGER "entero"
#define MS_MONEY "dinero"
#define MS_DECIMAL "decimal"
#define MS_DATE "fecha"
#define MS_DURATION "duración"
#define MR_DURATION "duraci", 0xF3, "n"
#define MS_BOOLEAN "booleano"
#define MS_POSITION "posición_fuente"
#define MR_POSITION "posici", 0xF3, "n_fuente"

#define MS_PRESENT "Presente"
#define MS_ABSENT "Ausente"
#define MS_Round "redondear"
#define MS_Impossible "imposible"
#define MS_Cardinal "número"
#define MR_Cardinal "n", 0xFA, "mero"

#define MR_LAW_INCLUDE "Inclusion"
#define MR_MODULE_DEF "Modulo"
#define MR_MODULE_USE "Uso", Star hspace, "de"
#define MR_MODULE_ALIAS "como"
#define MR_EXTERNAL "externo"
#define MX_AT_PAGE \
   '@', Star hspace, "p.", Star hspace, Plus digit -> \
      let s = Utf8.lexeme lexbuf in \
      let i = String.index s '.' in \
      AT_PAGE (int_of_string (String.trim (String.sub s i (String.length s - i))))
