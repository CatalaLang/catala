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

(* WARNING: this file must be saved as Latin-1 and not utf8, sedlex requires it *)

(* Defining the lexer macros for French *)


(* Tokens and their corresponding sedlex regexps *)

#define MS_SCOPE "champ d'application"
#define MR_SCOPE "champ", space_plus, "d'application"
#define MS_CONSEQUENCE "conséquence"
#define MS_DATA "donnée"
#define MS_DEPENDS "dépend de"
#define MR_DEPENDS "dépend", space_plus, "de"
#define MS_DECLARATION "déclaration"
#define MS_CONTEXT "contexte"
#define MS_DECREASING "décroissant"
#define MS_INCREASING "croissant"
#define MS_OF "de"
#define MS_COLLECTION "collection"
#define MS_ENUM "énumération"
#define MS_INTEGER "entier"
#define MS_MONEY "argent"
#define MS_TEXT "texte"
#define MS_DECIMAL "décimal"
#define MS_DATE "date"
#define MS_DURATION "durée"
#define MS_BOOLEAN "booléen"
#define MS_SUM "somme"
#define MS_FILLED "rempli"
#define MS_DEFINITION "définition"
#define MS_LABEL "étiquette"
#define MS_EXCEPTION "exception"
#define MS_DEFINED_AS "égal à"
#define MR_DEFINED_AS "égal", space_plus, "à"
#define MS_MATCH "selon"
#define MS_WILDCARD "n'importe quel"
#define MR_WILDCARD "n'importe", space_plus, "quel"
#define MS_WITH "sous forme"
#define MR_WITH "sous", space_plus, "forme"
#define MS_UNDER_CONDITION "sous condition"
#define MR_UNDER_CONDITION "sous", space_plus, "condition"
#define MS_IF "si"
#define MS_THEN "alors"
#define MS_ELSE "sinon"
#define MS_CONDITION "condition"
#define MS_CONTENT "contenu"
#define MS_STRUCT "structure"
#define MS_ASSERTION "assertion"
#define MS_VARIES "varie"
#define MS_WITH_V "avec"
#define MS_FOR "pour"
#define MS_ALL "tout"
#define MS_WE_HAVE "on a"
#define MR_WE_HAVE "on", space_plus, "a"
#define MS_FIXED "fixé"
#define MS_BY "par"
#define MS_RULE "règle"
#define MS_EXISTS "existe"
#define MS_IN "dans"
#define MS_SUCH "tel"
#define MS_THAT "que"
#define MS_AND "et"
#define MS_OR "ou"
#define MS_XOR "ou bien"
#define MR_XOR "ou", space_plus, "bien"
#define MS_NOT "non"
#define MS_MAXIMUM "maximum"
#define MS_MINIMUM "minimum"
#define MS_FILTER "filtre"
#define MS_MAP "application"
#define MS_INIT "initial"
#define MS_CARDINAL "nombre"
#define MS_YEAR "an"
#define MS_MONTH "mois"
#define MS_DAY "jour"
#define MS_TRUE "vrai"
#define MS_FALSE "faux"

(* Specific delimiters *)

#define MR_MONEY_OP_SUFFIX 0x20AC (* The euro sign *)
#define MC_DECIMAL_SEPARATOR ','
#define MR_MONEY_PREFIX ""
#define MR_MONEY_DELIM ' '
#define MR_MONEY_SUFFIX Star hspace, 0x20AC

(* Builtins *)

#define MS_IntToDec "entier_vers_décimal"
#define MS_GetDay "accès_jour"
#define MS_GetMonth "accès_mois"
#define MS_GetYear "accès_année"

(* Directives *)

#define MR_BEGIN_METADATA "Début", Plus hspace, "métadonnées"
#define MR_END_METADATA "Fin", Plus hspace, "métadonnées"
#define MR_LAW_INCLUDE "Inclusion"
#define MX_AT_PAGE \
   '@', Star hspace, "p.", Star hspace, Plus digit -> \
      let s = Utf8.lexeme lexbuf in \
      let i = String.index s '.' in \
      AT_PAGE (int_of_string (String.trim (String.sub s i (String.length s - i))))
