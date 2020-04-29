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
open Sedlexing
module R = Re.Pcre

let is_code : bool ref = ref false

let code_string_acc : string ref = ref ""

let update_acc (lexbuf : lexbuf) : unit = code_string_acc := !code_string_acc ^ Utf8.lexeme lexbuf

let token_list_en : (string * token) list =
  [
    ("application field", FIELD);
    ("consequence", CONSEQUENCE);
    ("data", DATA);
    ("depends on", DEPENDS);
    ("declaration", DECLARATION);
    ("context", CONTEXT);
    ("includes", INCLUDES);
    ("decreasing", DECREASING);
    ("increasing", INCREASING);
    ("of", OF);
    ("collection", COLLECTION);
    ("enumeration", ENUM);
    ("integer", INTEGER);
    ("amount", MONEY);
    ("text", TEXT);
    ("decimal", DECIMAL);
    ("date", DATE);
    ("boolean", BOOLEAN);
    ("sum", SUM);
    ("fulfilled", FILLED);
    ("definition", DEFINITION);
    ("equals", DEFINED_AS);
    ("match", MATCH);
    ("with pattern", WITH);
    ("under condition", UNDER_CONDITION);
    ("if", IF);
    ("then", THEN);
    ("else", ELSE);
    ("content", CONTENT);
    ("structure", STRUCT);
    ("optional", OPTIONAL);
    ("assertion", ASSERTION);
    ("varies", VARIES);
    ("with", WITH_V);
    ("for", FOR);
    ("all", ALL);
    ("we have", WE_HAVE);
    ("fixed", FIXED);
    ("by", BY);
    ("rule", RULE);
    ("exists", EXISTS);
    ("such", SUCH);
    ("that", THAT);
    ("now", NOW);
    ("and", AND);
    ("or", OR);
    ("not", NOT);
    ("number", CARDINAL);
    ("year", YEAR);
  ]
  @ Lexer_fr.token_list_language_agnostic

let rec lex_code_en (lexbuf : lexbuf) : token =
  match%sedlex lexbuf with
  | '\n' ->
      update_acc lexbuf;
      new_line lexbuf;
      lex_code_en lexbuf
  | white_space ->
      (* Whitespaces *)
      update_acc lexbuf;
      lex_code_en lexbuf
  | '#', Star (Compl '\n'), '\n' ->
      (* Comments *)
      update_acc lexbuf;
      new_line lexbuf;
      lex_code_en lexbuf
  | "*/" ->
      (* End of code section *)
      is_code := false;
      END_CODE !code_string_acc
  | "application field" ->
      update_acc lexbuf;
      FIELD
  | "data" ->
      update_acc lexbuf;
      DATA
  | "depends" ->
      update_acc lexbuf;
      DEPENDS
  | "declaration" ->
      update_acc lexbuf;
      DECLARATION
  | "context" ->
      update_acc lexbuf;
      CONTEXT
  | "includes" ->
      update_acc lexbuf;
      INCLUDES
  | "decreasing" ->
      update_acc lexbuf;
      DECREASING
  | "increasing" ->
      update_acc lexbuf;
      INCREASING
  | "of" ->
      update_acc lexbuf;
      OF
  | "collection" ->
      update_acc lexbuf;
      COLLECTION
  | "enumeration" ->
      update_acc lexbuf;
      ENUM
  | "integer" ->
      update_acc lexbuf;
      INTEGER
  | "amount" ->
      update_acc lexbuf;
      MONEY
  | "text" ->
      update_acc lexbuf;
      TEXT
  | "decimal" ->
      update_acc lexbuf;
      DECIMAL
  | "date" ->
      update_acc lexbuf;
      DATE
  | "boolean" ->
      update_acc lexbuf;
      BOOLEAN
  | "sum" ->
      update_acc lexbuf;
      SUM
  | "fulfilled" ->
      update_acc lexbuf;
      FILLED
  | "definition" ->
      update_acc lexbuf;
      DEFINITION
  | "equals" ->
      update_acc lexbuf;
      DEFINED_AS
  | "match" ->
      update_acc lexbuf;
      MATCH
  | "with pattern" ->
      update_acc lexbuf;
      WITH
  | "under condition" ->
      update_acc lexbuf;
      UNDER_CONDITION
  | "if" ->
      update_acc lexbuf;
      IF
  | "consequence" ->
      update_acc lexbuf;
      CONSEQUENCE
  | "then" ->
      update_acc lexbuf;
      THEN
  | "else" ->
      update_acc lexbuf;
      ELSE
  | "condition" ->
      update_acc lexbuf;
      CONDITION
  | "content" ->
      update_acc lexbuf;
      CONTENT
  | "structure" ->
      update_acc lexbuf;
      STRUCT
  | "optional" ->
      update_acc lexbuf;
      OPTIONAL
  | "assertion" ->
      update_acc lexbuf;
      ASSERTION
  | "varies" ->
      update_acc lexbuf;
      VARIES
  | "with" ->
      update_acc lexbuf;
      WITH_V
  | "for" ->
      update_acc lexbuf;
      FOR
  | "all" ->
      update_acc lexbuf;
      ALL
  | "we have" ->
      update_acc lexbuf;
      WE_HAVE
  | "fixed" ->
      update_acc lexbuf;
      FIXED
  | "by" ->
      update_acc lexbuf;
      BY
  | "rule" ->
      (* 0xE8 is è *)
      update_acc lexbuf;
      RULE
  | "exists" ->
      update_acc lexbuf;
      EXISTS
  | "in" ->
      update_acc lexbuf;
      IN
  | "such" ->
      update_acc lexbuf;
      SUCH
  | "that" ->
      update_acc lexbuf;
      THAT
  | "now" ->
      update_acc lexbuf;
      NOW
  | "and" ->
      update_acc lexbuf;
      AND
  | "or" ->
      update_acc lexbuf;
      OR
  | "not" ->
      update_acc lexbuf;
      NOT
  | "number" ->
      update_acc lexbuf;
      CARDINAL
  | "year" ->
      update_acc lexbuf;
      YEAR
  | 0x24, Star white_space, '0' .. '9', Star ('0' .. '9' | ','), Opt ('.', Rep ('0' .. '9', 0 .. 2))
    ->
      let extract_parts = R.regexp "([0-9]([0-9,]*[0-9]|))(.([0-9]{0,2})|)" in
      let full_str = Utf8.lexeme lexbuf in
      let only_numbers_str = String.trim (String.sub full_str 1 (String.length full_str - 1)) in
      let parts = R.get_substring (R.exec ~rex:extract_parts only_numbers_str) in
      (* Integer literal*)
      let units = parts 1 in
      let remove_commas = R.regexp "," in
      let units = int_of_string (R.substitute ~rex:remove_commas ~subst:(fun _ -> "") units) in
      let cents = try int_of_string (parts 4) with Not_found -> 0 in
      update_acc lexbuf;
      MONEY_AMOUNT (units, cents)
  | Plus '0' .. '9', '.', Star '0' .. '9' ->
      let extract_code_title = R.regexp "([0-9]+)\\.([0-9]*)" in
      let dec_parts = R.get_substring (R.exec ~rex:extract_code_title (Utf8.lexeme lexbuf)) in
      (* Integer literal*)
      update_acc lexbuf;
      DECIMAL_LITERAL (int_of_string (dec_parts 1), int_of_string (dec_parts 2))
  | "->" ->
      update_acc lexbuf;
      ARROW
  | '.' ->
      update_acc lexbuf;
      DOT
  | "<=" ->
      update_acc lexbuf;
      LESSER_EQUAL
  | '<' ->
      update_acc lexbuf;
      LESSER
  | ">=" ->
      update_acc lexbuf;
      GREATER_EQUAL
  | '>' ->
      update_acc lexbuf;
      GREATER
  | "!=" ->
      update_acc lexbuf;
      NOT_EQUAL
  | '=' ->
      update_acc lexbuf;
      EQUAL
  | '(' ->
      update_acc lexbuf;
      LPAREN
  | ')' ->
      update_acc lexbuf;
      RPAREN
  | '+' ->
      update_acc lexbuf;
      PLUS
  | '-' ->
      update_acc lexbuf;
      MINUS
  | '*' ->
      update_acc lexbuf;
      MULT
  | '%' ->
      update_acc lexbuf;
      PERCENT
  | '/' ->
      update_acc lexbuf;
      DIV
  | '|' ->
      update_acc lexbuf;
      VERTICAL
  | ':' ->
      update_acc lexbuf;
      COLON
  | "--" ->
      update_acc lexbuf;
      ALT
  | uppercase, Star (uppercase | lowercase | '0' .. '9' | '_' | '\'') ->
      (* Name of constructor *)
      update_acc lexbuf;
      CONSTRUCTOR (Utf8.lexeme lexbuf)
  | lowercase, Star (lowercase | uppercase | '0' .. '9' | '_' | '\'') ->
      (* Name of variable *)
      update_acc lexbuf;
      IDENT (Utf8.lexeme lexbuf)
  | Plus '0' .. '9' ->
      (* Integer literal*)
      update_acc lexbuf;
      INT_LITERAL (int_of_string (Utf8.lexeme lexbuf))
  | _ -> Errors.lexer_error (lexing_positions lexbuf) (Utf8.lexeme lexbuf)

let rec lex_law_en (lexbuf : lexbuf) : token =
  match%sedlex lexbuf with
  | '\n' ->
      new_line lexbuf;
      lex_law_en lexbuf
  | "/*" ->
      is_code := true;
      code_string_acc := "";
      BEGIN_CODE
  | eof -> EOF
  | "@@", Star white_space, "Master file", Star white_space, "@@" -> MASTER_FILE
  | "@@", Star white_space, "Begin metadata", Star white_space, "@@" -> BEGIN_METADATA
  | "@@", Star white_space, "End metadata", Star white_space, "@@" -> END_METADATA
  | ( "@@",
      Star white_space,
      "Include:",
      Star white_space,
      Plus (Compl '@'),
      Star white_space,
      Opt ('@', Star white_space, "p.", Star white_space, Plus '0' .. '9', Star white_space),
      "@@" ) ->
      let extract_components =
        R.regexp "@@\\s*Include\\:\\s*([^@]+)\\s*(@\\s*p\\.\\s*([0-9]+)|)@@"
      in
      let get_component = R.get_substring (R.exec ~rex:extract_components (Utf8.lexeme lexbuf)) in
      LAW_INCLUDE
        (get_component 1, try Some (int_of_string (get_component 3)) with Not_found -> None)
  | "@@", Plus (Compl '@'), "@@", Star '+' ->
      let extract_code_title = R.regexp "@@([^@]+)@@([\\+]*)" in
      let get_match = R.get_substring (R.exec ~rex:extract_code_title (Utf8.lexeme lexbuf)) in
      let get_new_lines = R.regexp "\n" in
      let new_lines_count =
        try Array.length (R.extract ~rex:get_new_lines (Utf8.lexeme lexbuf)) with Not_found -> 0
      in
      for _i = 1 to new_lines_count do
        new_line lexbuf
      done;
      let law_title = get_match 1 in
      let precedence = String.length (get_match 2) in

      LAW_HEADING (law_title, precedence)
  | "@", Plus (Compl '@'), "@" ->
      let extract_article_title = R.regexp "@([^@]+)@" in
      let title = R.get_substring (R.exec ~rex:extract_article_title (Utf8.lexeme lexbuf)) 1 in
      let get_new_lines = R.regexp "\n" in
      let new_lines_count =
        try Array.length (R.extract ~rex:get_new_lines (Utf8.lexeme lexbuf)) with Not_found -> 0
      in
      for _i = 1 to new_lines_count do
        new_line lexbuf
      done;

      LAW_ARTICLE (title, None, None)
  | Plus (Compl ('@' | '/' | '\n')) -> LAW_TEXT (Utf8.lexeme lexbuf)
  | _ -> Errors.lexer_error (lexing_positions lexbuf) (Utf8.lexeme lexbuf)

let lexer_en lexbuf = if !is_code then lex_code_en lexbuf else lex_law_en lexbuf
