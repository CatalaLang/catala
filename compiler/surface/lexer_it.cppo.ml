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

(* Defining the lexer macros for Italian *)

(* Tokens and their corresponding sedlex regexps *)

#define MS_SCOPE "campo di applicazione"
#define MR_SCOPE "campo", space_plus, "di", space_plus, "applicazione"
#define MS_CONSEQUENCE "conseguenza"
#define MS_DATA "dato"
#define MS_DEPENDS "dipende da"
#define MR_DEPENDS "dipende", space_plus, "da"
#define MS_DECLARATION "dichiarazione"
#define MS_CONTEXT "contesto"
#define MS_DECREASING "giù"
#define MR_DECREASING "gi", 0xF9
#define MS_INCREASING "su"
#define MS_OF "di"
#define MS_LIST "lista di"
#define MR_LIST "lista", space_plus, "di"
#define MS_OPTION "opzionale di"
#define MR_OPTION "opzionale", space_plus, "di"
#define MS_CONTAINS "contiene"
#define MS_ENUM "enumerazione"
#define MS_SUM "somma"
#define MS_FILLED "soddisfatto"
#define MS_DEFINITION "definizione"
#define MS_STATE "stato"
#define MS_LABEL "etichetta"
#define MS_EXCEPTION "eccezione"
#define MS_DEFINED_AS "vale"
#define MS_MATCH "secondo"
#define MS_WILDCARD "qualsiasi"
#define MS_TYPE "tipo"
#define MS_WITH "con forma"
#define MR_WITH "con", space_plus, "forma"
#define MS_UNDER_CONDITION "sotto condizione"
#define MR_UNDER_CONDITION "sotto", space_plus, "condizione"
#define MS_IF "se"
#define MS_THEN "allora"
#define MS_ELSE "altrimenti"
#define MS_CONDITION "condizione"
#define MS_CONTENT "contenuto"
#define MS_STRUCT "struttura"
#define MS_ASSERTION "asserzione"
#define MS_WITH_V "con"
#define MS_FOR "per"
#define MS_ALL "ogni"
#define MS_WE_HAVE "abbiamo"
#define MS_RULE "regola"
#define MS_LET "sia"
#define MS_EXISTS "esiste"
#define MS_IN "in"
#define MS_AMONG "tra"
#define MS_COMBINE "combina"
#define MS_MAP_EACH "trasforma ogni"
#define MR_MAP_EACH "trasforma", space_plus, "ogni"
#define MS_TO "a"
#define MS_SUCH "tale"
#define MS_THAT "che"
#define MS_SORT "ordina"
#define MS_ORDER_ASCENDING "in ordine crescente"
#define MR_ORDER_ASCENDING "in", space_plus, "ordine", space_plus, "crescente"
#define MS_ORDER_DESCENDING "in ordine decrescente"
#define MR_ORDER_DESCENDING "in", space_plus, "ordine", space_plus, "decrescente"
#define MS_AND_THEN "e poi"
#define MR_AND_THEN "e", space_plus, "poi"
#define MS_AND "e"
#define MS_OR "o"
#define MS_XOR "oppure"
#define MS_NOT "non"
#define MS_MAXIMUM "massimo"
#define MS_MINIMUM "minimo"
#define MS_IS "è"
#define MR_IS 0xE8
#define MS_OR_IF_LIST_EMPTY "o se lista vuota"
#define MR_OR_IF_LIST_EMPTY "o", space_plus, "se", space_plus, "lista", space_plus, "vuota"
#define MS_BUT_REPLACE "ma sostituendo"
#define MR_BUT_REPLACE "ma", space_plus, "sostituendo"
#define MS_INITIALLY "inizialmente"
#define MS_YEAR "anno"
#define MS_MONTH "mese"
#define MS_DAY "giorno"
#define MS_TRUE "vero"
#define MS_FALSE "falso"
#define MS_INPUT "entrata"
#define MS_OUTPUT "risultato"
#define MS_INTERNAL "interno"

(* Specific delimiters *)

#define MS_MONEY_OP_SUFFIX "€"
#define MR_MONEY_OP_SUFFIX 0x20AC
                           (* The euro sign *)
#define MC_DECIMAL_SEPARATOR ','
#define MR_MONEY_PREFIX ""
#define MR_MONEY_DELIM '.'
#define MR_MONEY_SUFFIX Star hspace, 0x20AC

(* Builtin types *)

#define MS_INTEGER "intero"
#define MS_MONEY "importo"
#define MS_DECIMAL "decimale"
#define MS_DATE "data"
#define MS_DURATION "durata"
#define MS_BOOLEAN "booleano"
#define MS_POSITION "posizione_sorgente"

(* Builtin constructors *)

#define MS_PRESENT "Presente"
#define MS_ABSENT "Assente"

(* Builtin functions *)

#define MS_Round "arrotondato"
#define MS_Impossible "impossibile"
#define MS_Cardinal "numero"

(* Directives *)

#define MR_LAW_INCLUDE "Inclusione"
#define MR_MODULE_DEF "Modulo"
#define MR_MODULE_USE "Uso", Star hspace, "di"
#define MR_MODULE_ALIAS "come"
#define MR_EXTERNAL "esterno"
#define MX_AT_PAGE \
   '@', Star hspace, "p.", Star hspace, Plus digit -> \
      let s = Utf8.lexeme lexbuf in \
      let i = String.index s '.' in \
      AT_PAGE (int_of_string (String.trim (String.sub s i (String.length s - i))))
