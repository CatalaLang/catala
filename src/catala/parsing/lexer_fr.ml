(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Parser
open Sedlex_menhir
module R = Re.Pcre

let is_code : bool ref = ref false

let code_string_acc : string ref = ref ""

let rec lex_code_as_string lexbuf acc =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | "*/" ->
      update lexbuf;
      END_CODE (acc ^ Sedlexing.Utf8.lexeme buf)
  | any ->
      update lexbuf;
      lex_code_as_string lexbuf (acc ^ Sedlexing.Utf8.lexeme buf)
  | _ -> raise_ParseError lexbuf

let update_and_acc lexbuf =
  update lexbuf;
  code_string_acc := !code_string_acc ^ Sedlexing.Utf8.lexeme lexbuf.stream

let rec lex_code_fr lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '\n' ->
      update_and_acc lexbuf;
      new_line lexbuf;
      lex_code_fr lexbuf
  | white_space ->
      (* Whitespaces *)
      update_and_acc lexbuf;
      lex_code_fr lexbuf
  | '#', Star (Compl '\n'), '\n' ->
      (* Comments *)
      update_and_acc lexbuf;
      new_line lexbuf;
      lex_code_fr lexbuf
  | "*/" ->
      (* End of code section *)
      update lexbuf;
      is_code := false;
      END_CODE !code_string_acc
  | "champ d\'application" ->
      update_and_acc lexbuf;
      FIELD
  | "donn", 0xE9, "e" ->
      (* 0xE9 is é *)
      update_and_acc lexbuf;
      DATA
  | "d", 0xE9, "pend de" ->
      update_and_acc lexbuf;
      DEPENDS
  | "d", 0xE9, "claration" ->
      update_and_acc lexbuf;
      DECLARATION
  | "contexte" ->
      update_and_acc lexbuf;
      CONTEXT
  | "inclus" ->
      update_and_acc lexbuf;
      INCLUDES
  | "d", 0xE9, "croissant" ->
      update_and_acc lexbuf;
      DECREASING
  | "croissant" ->
      update_and_acc lexbuf;
      INCREASING
  | "de" ->
      update_and_acc lexbuf;
      OF
  | "collection" ->
      update_and_acc lexbuf;
      COLLECTION
  | 0xE9, "num", 0xE9, "ration" ->
      update_and_acc lexbuf;
      ENUM
  | "entier" ->
      update_and_acc lexbuf;
      INTEGER
  | "montant" ->
      update_and_acc lexbuf;
      MONEY
  | "texte" ->
      update_and_acc lexbuf;
      TEXT
  | "d", 0xE9, "cimal" ->
      update_and_acc lexbuf;
      DECIMAL
  | "date" ->
      update_and_acc lexbuf;
      DATE
  | "bool", 0xE9, "en" ->
      update_and_acc lexbuf;
      BOOLEAN
  | "somme" ->
      update_and_acc lexbuf;
      SUM
  | "rempli" ->
      update_and_acc lexbuf;
      FILLED
  | "d", 0xE9, "finition" ->
      (* 0xE9 is é *)
      update_and_acc lexbuf;
      DEFINITION
  | 0xE9, "gal ", 0x00E0 ->
      (* 0xE9 is é *)
      update_and_acc lexbuf;
      DEFINED_AS
  | "selon" ->
      update_and_acc lexbuf;
      MATCH
  | "sous forme" ->
      update_and_acc lexbuf;
      WITH
  | "sous condition" ->
      update_and_acc lexbuf;
      UNDER_CONDITION
  | "si" ->
      update_and_acc lexbuf;
      IF
  | "cons", 0xE9, "quence" ->
      (* 0xE9 is é *)
      update_and_acc lexbuf;
      CONSEQUENCE
  | "alors" ->
      (* 0xE9 is é *)
      update_and_acc lexbuf;
      THEN
  | "sinon" ->
      update_and_acc lexbuf;
      ELSE
  | "condition" ->
      update_and_acc lexbuf;
      CONDITION
  | "contenu" ->
      update_and_acc lexbuf;
      CONTENT
  | "structure" ->
      update_and_acc lexbuf;
      STRUCT
  | "optionnel" ->
      update_and_acc lexbuf;
      OPTIONAL
  | "assertion" ->
      update_and_acc lexbuf;
      ASSERTION
  | "varie avec" ->
      update_and_acc lexbuf;
      VARIES_WITH
  | "pour" ->
      update_and_acc lexbuf;
      FOR
  | "tout" ->
      update_and_acc lexbuf;
      ALL
  | "on a" ->
      update_and_acc lexbuf;
      WE_HAVE
  | "fix", 0xE9 ->
      (* 0xE9 is é *)
      update_and_acc lexbuf;
      FIXED
  | "par" ->
      update_and_acc lexbuf;
      BY
  | "r", 0xE8, "gle" ->
      (* 0xE8 is è *)
      update_and_acc lexbuf;
      RULE
  | "existe" ->
      update_and_acc lexbuf;
      EXISTS
  | "dans" ->
      update_and_acc lexbuf;
      IN
  | "tel" ->
      update_and_acc lexbuf;
      SUCH
  | "que" ->
      update_and_acc lexbuf;
      THAT
  | "maintenant" ->
      update_and_acc lexbuf;
      NOW
  | "et" ->
      update_and_acc lexbuf;
      AND
  | "ou" ->
      update_and_acc lexbuf;
      OR
  | "non" ->
      update_and_acc lexbuf;
      NOT
  | "nombre" ->
      update_and_acc lexbuf;
      CARDINAL
  | "an" ->
      update_and_acc lexbuf;
      YEAR
  | Plus '0' .. '9', '.', Star '0' .. '9' ->
      let extract_code_title = R.regexp "([0-9]+)\\.([0-9]*)" in
      let dec_parts =
        R.get_substring (R.exec ~rex:extract_code_title (Sedlexing.Utf8.lexeme buf))
      in
      (* Integer literal*)
      update_and_acc lexbuf;
      DECIMAL_LITERAL (int_of_string (dec_parts 1), int_of_string (dec_parts 2))
  | "->" ->
      update_and_acc lexbuf;
      ARROW
  | '.' ->
      update_and_acc lexbuf;
      DOT
  | "<=" ->
      update_and_acc lexbuf;
      LESSER_EQUAL
  | '<' ->
      update_and_acc lexbuf;
      LESSER
  | ">=" ->
      update_and_acc lexbuf;
      GREATER_EQUAL
  | '>' ->
      update_and_acc lexbuf;
      GREATER
  | "!=" ->
      update_and_acc lexbuf;
      NOT_EQUAL
  | '=' ->
      update_and_acc lexbuf;
      EQUAL
  | '(' ->
      update_and_acc lexbuf;
      LPAREN
  | ')' ->
      update_and_acc lexbuf;
      RPAREN
  | '+' ->
      update_and_acc lexbuf;
      PLUS
  | '-' ->
      update_and_acc lexbuf;
      MINUS
  | '*' ->
      update_and_acc lexbuf;
      MULT
  | '%' ->
      update_and_acc lexbuf;
      PERCENT
  | '/' ->
      update_and_acc lexbuf;
      DIV
  | '|' ->
      update_and_acc lexbuf;
      VERTICAL
  | ':' ->
      update_and_acc lexbuf;
      COLON
  | "--" ->
      update_and_acc lexbuf;
      ALT
  | uppercase, Star (uppercase | lowercase | '0' .. '9' | '_' | '\'') ->
      (* Name of constructor *)
      update_and_acc lexbuf;
      CONSTRUCTOR (Sedlexing.Utf8.lexeme buf)
  | lowercase, Star (lowercase | uppercase | '0' .. '9' | '_' | '\'') ->
      (* Name of variable *)
      update_and_acc lexbuf;
      IDENT (Sedlexing.Utf8.lexeme buf)
  | Plus '0' .. '9' ->
      (* Integer literal*)
      update_and_acc lexbuf;
      INT_LITERAL (int_of_string (Sedlexing.Utf8.lexeme buf))
  | 0x20AC ->
      (* this is the euro sign € *)
      update_and_acc lexbuf;
      EURO
  | _ -> raise_ParseError lexbuf

let rec lex_law_fr lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '\n' ->
      update lexbuf;
      new_line lexbuf;
      lex_law_fr lexbuf
  | "/*" ->
      update lexbuf;
      is_code := true;
      code_string_acc := "";
      BEGIN_CODE
  | eof ->
      update lexbuf;
      EOF
  | "@@", Star white_space, "Fichier ma", 0x00EE, "tre", Star white_space, "@@" ->
      (* 0x00EE is î *)
      update lexbuf;
      MASTER_FILE
  | "@@", Star white_space, "D", 0xE9, "but m", 0xE9, "tadonn", 0xE9, "es", Star white_space, "@@"
    ->
      update lexbuf;
      BEGIN_METADATA
  | "@@", Star white_space, "Fin m", 0xE9, "tadonn", 0xE9, "es", Star white_space, "@@" ->
      update lexbuf;
      END_METADATA
  | ( "@@",
      Star white_space,
      "Inclusion:",
      Star white_space,
      Plus (Compl '@'),
      Star white_space,
      Opt ('@', Star white_space, "p.", Star white_space, Plus '0' .. '9', Star white_space),
      "@@" ) ->
      let extract_components =
        R.regexp "@@\\s*Inclusion\\:\\s*([^@]+)\\s*(@\\s*p\\.\\s*([0-9]+)|)@@"
      in
      let get_component =
        R.get_substring (R.exec ~rex:extract_components (Sedlexing.Utf8.lexeme buf))
      in
      update lexbuf;
      LAW_INCLUDE
        (get_component 1, try Some (int_of_string (get_component 3)) with Not_found -> None)
  | "@@", Plus (Compl '@'), "@@", Star '+' ->
      let extract_code_title = R.regexp "@@([^@]+)@@([\\+]*)" in
      let get_match =
        R.get_substring (R.exec ~rex:extract_code_title (Sedlexing.Utf8.lexeme buf))
      in
      let get_new_lines = R.regexp "\n" in
      let new_lines_count =
        try Array.length (R.extract ~rex:get_new_lines (Sedlexing.Utf8.lexeme buf))
        with Not_found -> 0
      in
      for _i = 1 to new_lines_count do
        new_line lexbuf
      done;
      let law_title = get_match 1 in
      let precedence = String.length (get_match 2) in
      update lexbuf;
      LAW_HEADING (law_title, precedence)
  | "@", Plus (Compl '@'), "@" ->
      let extract_article_title =
        R.regexp
          "\\@(([^\\|]+)\\|(((LEGIARTI|JORFARTI)[0-9]{12})(\\|([0-2]{2}\\/[0-2]{2}\\/[0-2]{4})|))|[^\\@]+)\\@"
      in
      let get_substring =
        R.get_substring (R.exec ~rex:extract_article_title (Sedlexing.Utf8.lexeme buf))
      in
      let title = try get_substring 2 with Not_found -> get_substring 1 in
      let article_id = try Some (get_substring 4) with Not_found -> None in
      let article_expiration_date = try Some (get_substring 7) with Not_found -> None in
      let get_new_lines = R.regexp "\n" in
      let new_lines_count =
        try Array.length (R.extract ~rex:get_new_lines (Sedlexing.Utf8.lexeme buf))
        with Not_found -> 0
      in
      for _i = 1 to new_lines_count do
        new_line lexbuf
      done;
      update lexbuf;
      LAW_ARTICLE (title, article_id, article_expiration_date)
  | Plus (Compl ('@' | '/' | '\n')) ->
      update lexbuf;
      LAW_TEXT (Sedlexing.Utf8.lexeme buf)
  | _ -> raise_ParseError lexbuf

let lexer_fr lexbuf = if !is_code then lex_code_fr lexbuf else lex_law_fr lexbuf
