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

open Tokens
open Sedlexing
open Utils
module L = Lexer_common
module R = Re.Pcre

(** Same as {!val: Surface.Lexer_common.token_list_language_agnostic}, but with tokens specialized
    to English. *)
let token_list : (string * token) list =
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
    ("anything", WILDCARD);
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
    ("xor", XOR);
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

(** Localised builtin functions *)
let builtins : (string * Ast.builtin_expression) list =
  [
    ("integer_to_decimal", IntToDec);
    ("get_day", GetDay);
    ("get_month", GetMonth);
    ("get_year", GetYear);
  ]

(** Regexp matching any digit character.

    @note can not be used outside the current module (@see <
    https://github.com/ocaml-community/sedlex#lexer-specifications >). *)
let digit = [%sedlex.regexp? '0' .. '9']

(** Regexp matching at least one space. *)
let space_plus = [%sedlex.regexp? Plus white_space]

(** Main lexing function used in code blocks *)
let rec lex_code (lexbuf : lexbuf) : token =
  let prev_lexeme = Utf8.lexeme lexbuf in
  let prev_pos = lexing_positions lexbuf in
  match%sedlex lexbuf with
  | white_space ->
      (* Whitespaces *)
      L.update_acc lexbuf;
      lex_code lexbuf
  | '#', Star (Compl '\n'), '\n' ->
      (* Comments *)
      L.update_acc lexbuf;
      lex_code lexbuf
  | "```" ->
      (* End of code section *)
      L.is_code := false;
      END_CODE !L.code_string_acc
  | "scope" ->
      L.update_acc lexbuf;
      SCOPE
  | "data" ->
      L.update_acc lexbuf;
      DATA
  | "depends", space_plus, "on" ->
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
  | "with", space_plus, "pattern" ->
      L.update_acc lexbuf;
      WITH
  | "anything" ->
      L.update_acc lexbuf;
      WILDCARD
  | "under", space_plus, "condition" ->
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
  | "we", space_plus, "have" ->
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
  | "xor" ->
      L.update_acc lexbuf;
      XOR
  | "not" ->
      L.update_acc lexbuf;
      NOT
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
  | 0x24, Star white_space, digit, Star (digit | ','), Opt ('.', Rep (digit, 0 .. 2)) ->
      let extract_parts = R.regexp "([0-9]([0-9,]*[0-9]|))(.([0-9]{0,2})|)" in
      let full_str = Utf8.lexeme lexbuf in
      let only_numbers_str = String.trim (String.sub full_str 1 (String.length full_str - 1)) in
      let parts = R.get_substring (R.exec ~rex:extract_parts only_numbers_str) in
      (* Integer literal*)
      let units = parts 1 in
      let remove_commas = R.regexp "," in
      let units =
        Runtime.integer_of_string (R.substitute ~rex:remove_commas ~subst:(fun _ -> "") units)
      in
      let cents =
        try Runtime.integer_of_string (parts 4) with Not_found -> Runtime.integer_of_int 0
      in
      L.update_acc lexbuf;
      MONEY_AMOUNT (units, cents)
  | Plus digit, '.', Star digit ->
      let extract_code_title = R.regexp "([0-9]+)\\.([0-9]*)" in
      let dec_parts = R.get_substring (R.exec ~rex:extract_code_title (Utf8.lexeme lexbuf)) in
      (* Integer literal*)
      L.update_acc lexbuf;
      DECIMAL_LITERAL
        (Runtime.integer_of_string (dec_parts 1), Runtime.integer_of_string (dec_parts 2))
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
  | "/^" ->
      L.update_acc lexbuf;
      DIVDURATION
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
  | uppercase, Star (uppercase | lowercase | digit | '_' | '\'') ->
      (* Name of constructor *)
      L.update_acc lexbuf;
      CONSTRUCTOR (Utf8.lexeme lexbuf)
  | lowercase, Star (lowercase | uppercase | digit | '_' | '\'') ->
      (* Name of variable *)
      L.update_acc lexbuf;
      IDENT (Utf8.lexeme lexbuf)
  | Plus digit ->
      (* Integer literal*)
      L.update_acc lexbuf;
      INT_LITERAL (Runtime.integer_of_string (Utf8.lexeme lexbuf))
  | _ -> L.raise_lexer_error (Pos.from_lpos prev_pos) prev_lexeme

(** Main lexing function used outside code blocks *)
let lex_law (lexbuf : lexbuf) : token =
  let prev_lexeme = Utf8.lexeme lexbuf in
  let prev_pos = lexing_positions lexbuf in
  let compl_catala =
    [%sedlex.regexp?
      ( Compl 'c'
      | 'c', Compl 'a'
      | "ca", Compl 't'
      | "cat", Compl 'a'
      | "cata", Compl 'l'
      | "catal", Compl 'a'
      | "catala", Compl (white_space | '\n') )]
  in
  match%sedlex lexbuf with
  | "```catala" ->
      L.is_code := true;
      L.code_string_acc := "";
      BEGIN_CODE
  | eof -> EOF
  | '>', Star white_space, "Begin metadata" -> BEGIN_METADATA
  | '>', Star white_space, "End metadata" -> END_METADATA
  | ( '>',
      Star white_space,
      "Include:",
      Star white_space,
      Plus (Compl ('@' | '\n')),
      Star white_space,
      Opt ('@', Star white_space, "p.", Star white_space, Plus digit, Star white_space),
      '\n' ) ->
      let extract_components =
        R.regexp ">\\s*Include\\:\\s*([^@\\n]+)\\s*(@\\s*p\\.\\s*([0-9]+)|)"
      in
      let get_component = R.get_substring (R.exec ~rex:extract_components (Utf8.lexeme lexbuf)) in
      let name = get_component 1 in
      let pages = try Some (int_of_string (get_component 3)) with Not_found -> None in
      let pos = lexing_positions lexbuf in
      if Filename.extension name = ".pdf" then
        LAW_INCLUDE (Ast.PdfFile ((name, Pos.from_lpos pos), pages))
      else LAW_INCLUDE (Ast.CatalaFile (name, Pos.from_lpos pos))
  | Plus '#', Star white_space, Plus (Compl '\n'), Star white_space, '\n' ->
      L.get_law_heading lexbuf
  | Plus
      (* Match non-special characters, i.e. characters that doesn't appear at the start of a
         previous regexp. *)
      ( Compl ('#' | '`' | '>')
      (* Following literals allow to match grave accents as long as they don't conflict with the
         [BEGIN_CODE] token, i.e. either there are no more than three consecutive ones or they must
         not be followed by 'catala'. *)
      | Rep ('`', 1 .. 2), Compl '`'
      | "```", compl_catala ) ->
      LAW_TEXT (Utf8.lexeme lexbuf)
  | _ -> L.raise_lexer_error (Pos.from_lpos prev_pos) prev_lexeme

(** Entry point of the lexer, distributes to {!val: lex_code} or {!val: lex_law} depending of {!val:
    Surface.Lexer_common.is_code}. *)
let lexer (lexbuf : lexbuf) : token = if !L.is_code then lex_code lexbuf else lex_law lexbuf
