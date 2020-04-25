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

let rec lex_code_en lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '\n' ->
      update_and_acc lexbuf;
      new_line lexbuf;
      lex_code_en lexbuf
  | white_space ->
      (* Whitespaces *)
      update_and_acc lexbuf;
      lex_code_en lexbuf
  | '#', Star (Compl '\n'), '\n' ->
      (* Comments *)
      update_and_acc lexbuf;
      new_line lexbuf;
      lex_code_en lexbuf
  | "*/" ->
      (* End of code section *)
      update lexbuf;
      is_code := false;
      END_CODE !code_string_acc
  | "application field" ->
      update_and_acc lexbuf;
      FIELD
  | "data" ->
      update_and_acc lexbuf;
      DATA
  | "depends" ->
      update_and_acc lexbuf;
      DEPENDS
  | "declaration" ->
      update_and_acc lexbuf;
      DECLARATION
  | "context" ->
      update_and_acc lexbuf;
      CONTEXT
  | "includes" ->
      update_and_acc lexbuf;
      INCLUDES
  | "decreasing" ->
      update_and_acc lexbuf;
      DECREASING
  | "increasing" ->
      update_and_acc lexbuf;
      INCREASING
  | "of" ->
      update_and_acc lexbuf;
      OF
  | "collection" ->
      update_and_acc lexbuf;
      COLLECTION
  | "enumeration" ->
      update_and_acc lexbuf;
      ENUM
  | "integer" ->
      update_and_acc lexbuf;
      INTEGER
  | "amount" ->
      update_and_acc lexbuf;
      MONEY
  | "text" ->
      update_and_acc lexbuf;
      TEXT
  | "decimal" ->
      update_and_acc lexbuf;
      DECIMAL
  | "date" ->
      update_and_acc lexbuf;
      DATE
  | "boolean" ->
      update_and_acc lexbuf;
      BOOLEAN
  | "sum" ->
      update_and_acc lexbuf;
      SUM
  | "fulfilled" ->
      update_and_acc lexbuf;
      FILLED
  | "definition" ->
      update_and_acc lexbuf;
      DEFINITION
  | "equals" ->
      update_and_acc lexbuf;
      DEFINED_AS
  | "match" ->
      update_and_acc lexbuf;
      MATCH
  | "with pattern" ->
      update_and_acc lexbuf;
      WITH
  | "under condition" ->
      update_and_acc lexbuf;
      UNDER_CONDITION
  | "if" ->
      update_and_acc lexbuf;
      IF
  | "consequence" ->
      update_and_acc lexbuf;
      CONSEQUENCE
  | "then" ->
      update_and_acc lexbuf;
      THEN
  | "else" ->
      update_and_acc lexbuf;
      ELSE
  | "condition" ->
      update_and_acc lexbuf;
      CONDITION
  | "content" ->
      update_and_acc lexbuf;
      CONTENT
  | "structure" ->
      update_and_acc lexbuf;
      STRUCT
  | "optional" ->
      update_and_acc lexbuf;
      OPTIONAL
  | "assertion" ->
      update_and_acc lexbuf;
      ASSERTION
  | "varies" ->
      update_and_acc lexbuf;
      VARIES
  | "with" ->
      update_and_acc lexbuf;
      WITH_V
  | "for" ->
      update_and_acc lexbuf;
      FOR
  | "all" ->
      update_and_acc lexbuf;
      ALL
  | "we have" ->
      update_and_acc lexbuf;
      WE_HAVE
  | "fixed" ->
      update_and_acc lexbuf;
      FIXED
  | "by" ->
      update_and_acc lexbuf;
      BY
  | "rule" ->
      (* 0xE8 is è *)
      update_and_acc lexbuf;
      RULE
  | "exists" ->
      update_and_acc lexbuf;
      EXISTS
  | "in" ->
      update_and_acc lexbuf;
      IN
  | "such" ->
      update_and_acc lexbuf;
      SUCH
  | "that" ->
      update_and_acc lexbuf;
      THAT
  | "now" ->
      update_and_acc lexbuf;
      NOW
  | "and" ->
      update_and_acc lexbuf;
      AND
  | "or" ->
      update_and_acc lexbuf;
      OR
  | "not" ->
      update_and_acc lexbuf;
      NOT
  | "number" ->
      update_and_acc lexbuf;
      CARDINAL
  | "year" ->
      update_and_acc lexbuf;
      YEAR
  | 0x24, Star white_space, '0' .. '9', Star ('0' .. '9' | ','), Opt ('.', Rep ('0' .. '9', 0 .. 2))
    ->
      let extract_parts = R.regexp "([0-9]([0-9,]*[0-9]|))(.([0-9]{0,2})|)" in
      let full_str = Sedlexing.Utf8.lexeme buf in
      let only_numbers_str = String.trim (String.sub full_str 1 (String.length full_str - 1)) in
      let parts = R.get_substring (R.exec ~rex:extract_parts only_numbers_str) in
      (* Integer literal*)
      let units = parts 1 in
      let remove_commas = R.regexp "," in
      let units = int_of_string (R.substitute ~rex:remove_commas ~subst:(fun _ -> "") units) in
      let cents = try int_of_string (parts 4) with Not_found -> 0 in
      MONEY_AMOUNT (units, cents)
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
  | _ -> raise_ParseError lexbuf

let rec lex_law_en lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '\n' ->
      update lexbuf;
      new_line lexbuf;
      lex_law_en lexbuf
  | "/*" ->
      update lexbuf;
      is_code := true;
      code_string_acc := "";
      BEGIN_CODE
  | eof ->
      update lexbuf;
      EOF
  | "@@", Star white_space, "Master file", Star white_space, "@@" ->
      update lexbuf;
      MASTER_FILE
  | "@@", Star white_space, "Begin metadata", Star white_space, "@@" ->
      update lexbuf;
      BEGIN_METADATA
  | "@@", Star white_space, "End metadata", Star white_space, "@@" ->
      update lexbuf;
      END_METADATA
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
      let extract_article_title = R.regexp "@([^@]+)@" in
      let title =
        R.get_substring (R.exec ~rex:extract_article_title (Sedlexing.Utf8.lexeme buf)) 1
      in
      let get_new_lines = R.regexp "\n" in
      let new_lines_count =
        try Array.length (R.extract ~rex:get_new_lines (Sedlexing.Utf8.lexeme buf))
        with Not_found -> 0
      in
      for _i = 1 to new_lines_count do
        new_line lexbuf
      done;
      update lexbuf;
      LAW_ARTICLE (title, None, None)
  | Plus (Compl ('@' | '/' | '\n')) ->
      update lexbuf;
      LAW_TEXT (Sedlexing.Utf8.lexeme buf)
  | _ -> raise_ParseError lexbuf

let lexer_en lexbuf = if !is_code then lex_code_en lexbuf else lex_law_en lexbuf
