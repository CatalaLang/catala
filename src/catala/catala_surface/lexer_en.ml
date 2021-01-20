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
module Pos = Utils.Pos
module Errors = Utils.Errors
module L = Lexer
module R = Re.Pcre

(** Same as {!val: Surface.Lexer.token_list_language_agnostic}, but with tokens specialized to
    English. *)
let token_list_en : (string * token) list =
  [
    ("scope", SCOPE);
    ("consequence", CONSEQUENCE);
    ("data", DATA);
    ("depends on", DEPENDS);
    ("declaration", DECLARATION);
    ("context", CONTEXT);
    ("decreasing", DECREASING);
    ("increasing", INCREASING);
    ("of", OF);
    ("collection", COLLECTION);
    ("enumeration", ENUM);
    ("integer", INTEGER);
    ("money", MONEY);
    ("text", TEXT);
    ("decimal", DECIMAL);
    ("date", DATE);
    ("duration", DURATION);
    ("boolean", BOOLEAN);
    ("sum", SUM);
    ("fulfilled", FILLED);
    ("definition", DEFINITION);
    ("label", LABEL);
    ("exception", EXCEPTION);
    ("equals", DEFINED_AS);
    ("match", MATCH);
    ("with pattern", WITH);
    ("under condition", UNDER_CONDITION);
    ("if", IF);
    ("then", THEN);
    ("else", ELSE);
    ("content", CONTENT);
    ("structure", STRUCT);
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
    ("and", AND);
    ("or", OR);
    ("not", NOT);
    ("maximum", MAXIMUM);
    ("minimum", MINIMUM);
    ("filter", FILTER);
    ("map", MAP);
    ("initial", INIT);
    ("number", CARDINAL);
    ("year", YEAR);
    ("month", MONTH);
    ("day", DAY);
    ("true", TRUE);
    ("false", FALSE);
  ]
  @ L.token_list_language_agnostic

(** Main lexing function used in code blocks *)
let rec lex_code_en (lexbuf : lexbuf) : token =
  let prev_lexeme = Utf8.lexeme lexbuf in
  let prev_pos = lexing_positions lexbuf in
  match%sedlex lexbuf with
  | white_space ->
      (* Whitespaces *)
      L.update_acc lexbuf;
      lex_code_en lexbuf
  | '#', Star (Compl '\n'), '\n' ->
      (* Comments *)
      L.update_acc lexbuf;
      lex_code_en lexbuf
  | "*/" ->
      (* End of code section *)
      L.is_code := false;
      END_CODE !L.code_string_acc
  | "scope" ->
      L.update_acc lexbuf;
      SCOPE
  | "data" ->
      L.update_acc lexbuf;
      DATA
  | "depends on" ->
      L.update_acc lexbuf;
      DEPENDS
  | "declaration" ->
      L.update_acc lexbuf;
      DECLARATION
  | "context" ->
      L.update_acc lexbuf;
      CONTEXT
  | "decreasing" ->
      L.update_acc lexbuf;
      DECREASING
  | "increasing" ->
      L.update_acc lexbuf;
      INCREASING
  | "of" ->
      L.update_acc lexbuf;
      OF
  | "collection" ->
      L.update_acc lexbuf;
      COLLECTION
  | "enumeration" ->
      L.update_acc lexbuf;
      ENUM
  | "integer" ->
      L.update_acc lexbuf;
      INTEGER
  | "money" ->
      L.update_acc lexbuf;
      MONEY
  | "text" ->
      L.update_acc lexbuf;
      TEXT
  | "decimal" ->
      L.update_acc lexbuf;
      DECIMAL
  | "date" ->
      L.update_acc lexbuf;
      DATE
  | "duration" ->
      L.update_acc lexbuf;
      DURATION
  | "boolean" ->
      L.update_acc lexbuf;
      BOOLEAN
  | "sum" ->
      L.update_acc lexbuf;
      SUM
  | "fulfilled" ->
      L.update_acc lexbuf;
      FILLED
  | "definition" ->
      L.update_acc lexbuf;
      DEFINITION
  | "label" ->
      L.update_acc lexbuf;
      LABEL
  | "exception" ->
      L.update_acc lexbuf;
      EXCEPTION
  | "equals" ->
      L.update_acc lexbuf;
      DEFINED_AS
  | "match" ->
      L.update_acc lexbuf;
      MATCH
  | "with pattern" ->
      L.update_acc lexbuf;
      WITH
  | "under condition" ->
      L.update_acc lexbuf;
      UNDER_CONDITION
  | "if" ->
      L.update_acc lexbuf;
      IF
  | "consequence" ->
      L.update_acc lexbuf;
      CONSEQUENCE
  | "then" ->
      L.update_acc lexbuf;
      THEN
  | "else" ->
      L.update_acc lexbuf;
      ELSE
  | "condition" ->
      L.update_acc lexbuf;
      CONDITION
  | "content" ->
      L.update_acc lexbuf;
      CONTENT
  | "structure" ->
      L.update_acc lexbuf;
      STRUCT
  | "assertion" ->
      L.update_acc lexbuf;
      ASSERTION
  | "varies" ->
      L.update_acc lexbuf;
      VARIES
  | "with" ->
      L.update_acc lexbuf;
      WITH_V
  | "for" ->
      L.update_acc lexbuf;
      FOR
  | "all" ->
      L.update_acc lexbuf;
      ALL
  | "we have" ->
      L.update_acc lexbuf;
      WE_HAVE
  | "fixed" ->
      L.update_acc lexbuf;
      FIXED
  | "by" ->
      L.update_acc lexbuf;
      BY
  | "rule" ->
      (* 0xE8 is è *)
      L.update_acc lexbuf;
      RULE
  | "exists" ->
      L.update_acc lexbuf;
      EXISTS
  | "in" ->
      L.update_acc lexbuf;
      IN
  | "such" ->
      L.update_acc lexbuf;
      SUCH
  | "that" ->
      L.update_acc lexbuf;
      THAT
  | "and" ->
      L.update_acc lexbuf;
      AND
  | "or" ->
      L.update_acc lexbuf;
      OR
  | "not" ->
      L.update_acc lexbuf;
      NOT
  | "integer_to_decimal" ->
      L.update_acc lexbuf;
      INT_TO_DEC
  | "get_day" ->
      L.update_acc lexbuf;
      GET_DAY
  | "get_month" ->
      L.update_acc lexbuf;
      GET_MONTH
  | "get_year" ->
      L.update_acc lexbuf;
      GET_YEAR
  | "maximum" ->
      L.update_acc lexbuf;
      MAXIMUM
  | "minimum" ->
      L.update_acc lexbuf;
      MINIMUM
  | "filter" ->
      L.update_acc lexbuf;
      FILTER
  | "map" ->
      L.update_acc lexbuf;
      MAP
  | "initial" ->
      L.update_acc lexbuf;
      INIT
  | "number" ->
      L.update_acc lexbuf;
      CARDINAL
  | "true" ->
      L.update_acc lexbuf;
      TRUE
  | "false" ->
      L.update_acc lexbuf;
      FALSE
  | "year" ->
      L.update_acc lexbuf;
      YEAR
  | "month" ->
      L.update_acc lexbuf;
      MONTH
  | "day" ->
      L.update_acc lexbuf;
      DAY
  | 0x24, Star white_space, '0' .. '9', Star ('0' .. '9' | ','), Opt ('.', Rep ('0' .. '9', 0 .. 2))
    ->
      let extract_parts = R.regexp "([0-9]([0-9,]*[0-9]|))(.([0-9]{0,2})|)" in
      let full_str = Utf8.lexeme lexbuf in
      let only_numbers_str = String.trim (String.sub full_str 1 (String.length full_str - 1)) in
      let parts = R.get_substring (R.exec ~rex:extract_parts only_numbers_str) in
      (* Integer literal*)
      let units = parts 1 in
      let remove_commas = R.regexp "," in
      let units = Z.of_string (R.substitute ~rex:remove_commas ~subst:(fun _ -> "") units) in
      let cents = try Z.of_string (parts 4) with Not_found -> Z.zero in
      L.update_acc lexbuf;
      MONEY_AMOUNT (units, cents)
  | Plus '0' .. '9', '.', Star '0' .. '9' ->
      let extract_code_title = R.regexp "([0-9]+)\\.([0-9]*)" in
      let dec_parts = R.get_substring (R.exec ~rex:extract_code_title (Utf8.lexeme lexbuf)) in
      (* Integer literal*)
      L.update_acc lexbuf;
      DECIMAL_LITERAL (Z.of_string (dec_parts 1), Z.of_string (dec_parts 2))
  | "->" ->
      L.update_acc lexbuf;
      ARROW
  | "<=@" ->
      L.update_acc lexbuf;
      LESSER_EQUAL_DATE
  | "<@" ->
      L.update_acc lexbuf;
      LESSER_DATE
  | ">=@" ->
      L.update_acc lexbuf;
      GREATER_EQUAL_DATE
  | ">@" ->
      L.update_acc lexbuf;
      GREATER_DATE
  | "-@" ->
      L.update_acc lexbuf;
      MINUSDATE
  | "+@" ->
      L.update_acc lexbuf;
      PLUSDATE
  | "<=^" ->
      L.update_acc lexbuf;
      LESSER_EQUAL_DURATION
  | "<^" ->
      L.update_acc lexbuf;
      LESSER_DURATION
  | ">=^" ->
      L.update_acc lexbuf;
      GREATER_EQUAL_DURATION
  | ">^" ->
      L.update_acc lexbuf;
      GREATER_DURATION
  | "+^" ->
      L.update_acc lexbuf;
      PLUSDURATION
  | "-^" ->
      L.update_acc lexbuf;
      MINUSDURATION
  | "<=", 0x24 ->
      L.update_acc lexbuf;
      LESSER_EQUAL_MONEY
  | '<', 0x24 ->
      L.update_acc lexbuf;
      LESSER_MONEY
  | ">=", 0x24 ->
      L.update_acc lexbuf;
      GREATER_EQUAL_MONEY
  | '>', 0x24 ->
      L.update_acc lexbuf;
      GREATER_MONEY
  | '+', 0x24 ->
      L.update_acc lexbuf;
      PLUSMONEY
  | '-', 0x24 ->
      L.update_acc lexbuf;
      MINUSMONEY
  | '*', 0x24 ->
      L.update_acc lexbuf;
      MULTMONEY
  | '/', 0x24 ->
      L.update_acc lexbuf;
      DIVMONEY
  | "<=." ->
      L.update_acc lexbuf;
      LESSER_EQUAL_DEC
  | "<." ->
      L.update_acc lexbuf;
      LESSER_DEC
  | ">=." ->
      L.update_acc lexbuf;
      GREATER_EQUAL_DEC
  | ">." ->
      L.update_acc lexbuf;
      GREATER_DEC
  | "+." ->
      L.update_acc lexbuf;
      PLUSDEC
  | "-." ->
      L.update_acc lexbuf;
      MINUSDEC
  | "*." ->
      L.update_acc lexbuf;
      MULTDEC
  | "/." ->
      L.update_acc lexbuf;
      DIVDEC
  | "<=" ->
      L.update_acc lexbuf;
      LESSER_EQUAL
  | '<' ->
      L.update_acc lexbuf;
      LESSER
  | ">=" ->
      L.update_acc lexbuf;
      GREATER_EQUAL
  | '>' ->
      L.update_acc lexbuf;
      GREATER
  | '+' ->
      L.update_acc lexbuf;
      PLUS
  | '-' ->
      L.update_acc lexbuf;
      MINUS
  | '*' ->
      L.update_acc lexbuf;
      MULT
  | '/' ->
      L.update_acc lexbuf;
      DIV
  | "!=" ->
      L.update_acc lexbuf;
      NOT_EQUAL
  | '=' ->
      L.update_acc lexbuf;
      EQUAL
  | '%' ->
      L.update_acc lexbuf;
      PERCENT
  | '(' ->
      L.update_acc lexbuf;
      LPAREN
  | ')' ->
      L.update_acc lexbuf;
      RPAREN
  | '{' ->
      L.update_acc lexbuf;
      LBRACKET
  | '}' ->
      L.update_acc lexbuf;
      RBRACKET
  | '[' ->
      L.update_acc lexbuf;
      LSQUARE
  | ']' ->
      L.update_acc lexbuf;
      RSQUARE
  | '|' ->
      L.update_acc lexbuf;
      VERTICAL
  | ':' ->
      L.update_acc lexbuf;
      COLON
  | ';' ->
      L.update_acc lexbuf;
      SEMICOLON
  | "--" ->
      L.update_acc lexbuf;
      ALT
  | '.' ->
      L.update_acc lexbuf;
      DOT
  | uppercase, Star (uppercase | lowercase | '0' .. '9' | '_' | '\'') ->
      (* Name of constructor *)
      L.update_acc lexbuf;
      CONSTRUCTOR (Utf8.lexeme lexbuf)
  | lowercase, Star (lowercase | uppercase | '0' .. '9' | '_' | '\'') ->
      (* Name of variable *)
      L.update_acc lexbuf;
      IDENT (Utf8.lexeme lexbuf)
  | Plus '0' .. '9' ->
      (* Integer literal*)
      L.update_acc lexbuf;
      INT_LITERAL (Z.of_string (Utf8.lexeme lexbuf))
  | _ -> L.raise_lexer_error (Pos.from_lpos prev_pos) prev_lexeme

(** Main lexing function used outside code blocks *)
let lex_law_en (lexbuf : lexbuf) : token =
  let prev_lexeme = Utf8.lexeme lexbuf in
  let prev_pos = lexing_positions lexbuf in
  match%sedlex lexbuf with
  | "/*" ->
      L.is_code := true;
      L.code_string_acc := "";
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
      let name = get_component 1 in
      let pages = try Some (int_of_string (get_component 3)) with Not_found -> None in
      let pos = lexing_positions lexbuf in
      if Filename.extension name = ".pdf" then
        LAW_INCLUDE (Ast.PdfFile ((name, Pos.from_lpos pos), pages))
      else LAW_INCLUDE (Ast.CatalaFile (name, Pos.from_lpos pos))
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
      (* the -1 is here to compensate for Sedlex's automatic newline detection around token *)
      for _i = 1 to new_lines_count - 1 do
        new_line lexbuf
      done;

      LAW_ARTICLE (title, None, None)
  | Plus (Compl ('@' | '/')) -> LAW_TEXT (Utf8.lexeme lexbuf)
  | _ -> L.raise_lexer_error (Pos.from_lpos prev_pos) prev_lexeme

let lexer_en lexbuf = if !L.is_code then lex_code_en lexbuf else lex_law_en lexbuf
