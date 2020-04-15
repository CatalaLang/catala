(* This file is part of the Lawspec compiler, a specification language for tax and social benefits
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

let rec lex_code lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '\n' ->
      (* Whitespaces *)
      update_and_acc lexbuf;
      new_line lexbuf;
      lex_code lexbuf
  | white_space ->
      update_and_acc lexbuf;
      lex_code lexbuf
  | '#', Star (Compl '\n'), '\n' ->
      (* Comments *)
      update_and_acc lexbuf;
      new_line lexbuf;
      lex_code lexbuf
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
  | "d", 0xE9, "pend" ->
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
      update lexbuf;
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

let rec lex_law lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '\n' ->
      update lexbuf;
      new_line lexbuf;
      lex_law lexbuf
  | "/*" ->
      update lexbuf;
      is_code := true;
      code_string_acc := "";
      BEGIN_CODE
  | eof ->
      update lexbuf;
      EOF
  | "@@", Star white_space, "D", 0xE9, "but m", 0xE9, "tadonn", 0xE9, "es", Star white_space, "@@"
    ->
      update lexbuf;
      BEGIN_METADATA
  | "@@", Star white_space, "Fin m", 0xE9, "tadonn", 0xE9, "es", Star white_space, "@@" ->
      update lexbuf;
      END_METADATA
  | "@@", Star white_space, Star (Compl '@'), Star white_space, "@@" ->
      let extract_code_title = R.regexp "@@\\s*([^#]*)\\s*@@" in
      let title = R.get_substring (R.exec ~rex:extract_code_title (Sedlexing.Utf8.lexeme buf)) 1 in
      update lexbuf;
      LAW_CODE title
  | "@", Star white_space, Star (Compl '@'), Star white_space, "@" ->
      let extract_article_title = R.regexp "@\\s*([^#]*)\\s*@" in
      let title =
        R.get_substring (R.exec ~rex:extract_article_title (Sedlexing.Utf8.lexeme buf)) 1
      in
      update lexbuf;
      LAW_ARTICLE title
  | Plus (Compl ('@' | '/' | '\n')) ->
      update lexbuf;
      LAW_TEXT (Sedlexing.Utf8.lexeme buf)
  | _ -> raise_ParseError lexbuf

let lexer lexbuf = if !is_code then lex_code lexbuf else lex_law lexbuf
