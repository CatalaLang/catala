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

(** Concise syntax with English abbreviated keywords. *)

open Tokens
open Sedlexing
open Utils
open Lexer_common
module R = Re.Pcre

(** Boolean reference, used by the lexer as the mutable state to distinguish whether it is lexing
    code or law. *)
let is_code : bool ref = ref false

(** Mutable string reference that accumulates the string representation of the body of code being
    lexed. This string representation is used in the literate programming backends to faithfully
    capture the spacing pattern of the original program *)
let code_string_acc : string ref = ref ""

(** Updates {!val:code_string_acc} with the current lexeme *)
let update_acc (lexbuf : lexbuf) : unit = code_string_acc := !code_string_acc ^ Utf8.lexeme lexbuf

(** Error-generating helper *)
let raise_lexer_error (loc : Pos.t) (token : string) =
  Errors.raise_spanned_error
    (Printf.sprintf "Parsing error after token \"%s\": what comes after is unknown" token)
    loc

(** Associative list matching each punctuation string part of the Catala syntax with its {!module:
    Surface.Parser} token. Same for all the input languages (English, French, etc.) *)
let token_list_language_agnostic : (string * token) list =
  [
    (".", DOT);
    ("<=", LESSER_EQUAL);
    (">=", GREATER_EQUAL);
    (">", GREATER);
    ("!=", NOT_EQUAL);
    ("=", EQUAL);
    ("(", LPAREN);
    (")", RPAREN);
    ("{", LBRACKET);
    ("}", RBRACKET);
    ("{", LSQUARE);
    ("}", RSQUARE);
    ("+", PLUS);
    ("-", MINUS);
    ("*", MULT);
    ("/", DIV);
    ("|", VERTICAL);
    (":", COLON);
    (";", SEMICOLON);
    ("--", ALT);
  ]

(** Same as {!val: token_list_language_agnostic}, but with tokens whose string varies with the input
    language. *)
let token_list : (string * token) list =
  [
    ("scope", SCOPE);
    ("|]", CONSEQUENCE);
    ("data", DATA);
    ("fun of", DEPENDS);
    ("new", DECLARATION);
    ("param", CONTEXT);
    ("decreasing", DECREASING);
    ("increasing", INCREASING);
    ("maximum", MAXIMUM);
    ("minimum", MINIMUM);
    ("filter", FILTER);
    ("map", MAP);
    ("init", INIT);
    ("of", OF);
    ("set", COLLECTION);
    ("enum", ENUM);
    ("int", INTEGER);
    ("money", MONEY);
    ("text", TEXT);
    ("decimal", DECIMAL);
    ("date", DATE);
    ("duration", DURATION);
    ("boolean", BOOLEAN);
    ("sum", SUM);
    ("ok", FILLED);
    ("def", DEFINITION);
    ("label", LABEL);
    ("exception", EXCEPTION);
    ("equals", DEFINED_AS);
    ("match", MATCH);
    ("with", WITH);
    ("[|", UNDER_CONDITION);
    ("if", IF);
    ("then", THEN);
    ("else", ELSE);
    ("content", CONTENT);
    ("struct", STRUCT);
    ("assert", ASSERTION);
    ("varies", VARIES);
    ("with parameter", WITH_V);
    ("for", FOR);
    ("all", ALL);
    ("we have", WE_HAVE);
    ("fixed", FIXED);
    ("by", BY);
    ("rule", RULE);
    ("exists", EXISTS);
    ("such", SUCH);
    ("that", THAT);
    ("&&", AND);
    ("||", OR);
    ("xor", XOR);
    ("not", NOT);
    ("number", CARDINAL);
    ("year", YEAR);
    ("month", MONTH);
    ("day", DAY);
    ("true", TRUE);
    ("false", FALSE);
  ]
  @ token_list_language_agnostic

(** Localised builtin functions *)
let builtins : (string * Ast.builtin_expression) list =
  [ ("int_to_dec", IntToDec); ("get_day", GetDay); ("get_month", GetMonth); ("get_year", GetYear) ]

(** Regexp matching any digit character. *)
let digit = [%sedlex.regexp? '0' .. '9']

(** Main lexing function used in a code block *)
let rec lex_code (lexbuf : lexbuf) : token =
  let prev_lexeme = Utf8.lexeme lexbuf in
  let prev_pos = lexing_positions lexbuf in
  match%sedlex lexbuf with
  | white_space ->
      (* Whitespaces *)
      update_acc lexbuf;
      lex_code lexbuf
  | '#', Star (Compl '\n'), '\n' ->
      (* Comments *)
      update_acc lexbuf;
      lex_code lexbuf
  | "```" ->
      (* End of code section *)
      is_code := false;
      END_CODE !code_string_acc
  | "scope" ->
      update_acc lexbuf;
      SCOPE
  | "data" ->
      update_acc lexbuf;
      DATA
  | "fun of" ->
      update_acc lexbuf;
      DEPENDS
  | "new" ->
      update_acc lexbuf;
      DECLARATION
  | "param" ->
      update_acc lexbuf;
      CONTEXT
  | "decreasing" ->
      update_acc lexbuf;
      DECREASING
  | "increasing" ->
      update_acc lexbuf;
      INCREASING
  | "of" ->
      update_acc lexbuf;
      OF
  | "set" ->
      update_acc lexbuf;
      COLLECTION
  | "enum" ->
      update_acc lexbuf;
      ENUM
  | "int" ->
      update_acc lexbuf;
      INTEGER
  | "money" ->
      update_acc lexbuf;
      MONEY
  | "text" ->
      update_acc lexbuf;
      TEXT
  | "dec" ->
      update_acc lexbuf;
      DECIMAL
  | "date" ->
      update_acc lexbuf;
      DATE
  | "duration" ->
      update_acc lexbuf;
      DURATION
  | "bool" ->
      update_acc lexbuf;
      BOOLEAN
  | "sum" ->
      update_acc lexbuf;
      SUM
  | "ok" ->
      update_acc lexbuf;
      FILLED
  | "def" ->
      update_acc lexbuf;
      DEFINITION
  | "label" ->
      update_acc lexbuf;
      LABEL
  | "exception" ->
      update_acc lexbuf;
      EXCEPTION
  | ":=" ->
      update_acc lexbuf;
      DEFINED_AS
  | "varies" ->
      update_acc lexbuf;
      VARIES
  | "withv" ->
      update_acc lexbuf;
      WITH_V
  | "match" ->
      update_acc lexbuf;
      MATCH
  | "with" ->
      update_acc lexbuf;
      WITH
  | "[|" ->
      update_acc lexbuf;
      UNDER_CONDITION
  | "if" ->
      update_acc lexbuf;
      IF
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
  | "struct" ->
      update_acc lexbuf;
      STRUCT
  | "assert" ->
      update_acc lexbuf;
      ASSERTION
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
  | "&&" ->
      update_acc lexbuf;
      AND
  | "||" ->
      update_acc lexbuf;
      OR
  | "xor" ->
      update_acc lexbuf;
      XOR
  | "not" ->
      update_acc lexbuf;
      NOT
  | "|]" ->
      update_acc lexbuf;
      CONSEQUENCE
  | "maximum" ->
      update_acc lexbuf;
      MAXIMUM
  | "minimum" ->
      update_acc lexbuf;
      MINIMUM
  | "filter" ->
      update_acc lexbuf;
      FILTER
  | "map" ->
      update_acc lexbuf;
      MAP
  | "init" ->
      update_acc lexbuf;
      INIT
  | "number" ->
      update_acc lexbuf;
      CARDINAL
  | "true" ->
      update_acc lexbuf;
      TRUE
  | "false" ->
      update_acc lexbuf;
      FALSE
  | "year" ->
      update_acc lexbuf;
      YEAR
  | "month" ->
      update_acc lexbuf;
      MONTH
  | "day" ->
      update_acc lexbuf;
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
      update_acc lexbuf;
      MONEY_AMOUNT (units, cents)
  | Plus digit, '.', Star digit ->
      let extract_code_title = R.regexp "([0-9]+)\\.([0-9]*)" in
      let dec_parts = R.get_substring (R.exec ~rex:extract_code_title (Utf8.lexeme lexbuf)) in
      (* Integer literal*)
      update_acc lexbuf;
      DECIMAL_LITERAL
        (Runtime.integer_of_string (dec_parts 1), Runtime.integer_of_string (dec_parts 2))
  | "<=@" ->
      update_acc lexbuf;
      LESSER_EQUAL_DATE
  | "<@" ->
      update_acc lexbuf;
      LESSER_DATE
  | ">=@" ->
      update_acc lexbuf;
      GREATER_EQUAL_DATE
  | ">@" ->
      update_acc lexbuf;
      GREATER_DATE
  | "-@" ->
      update_acc lexbuf;
      MINUSDATE
  | "+@" ->
      update_acc lexbuf;
      PLUSDATE
  | "<=^" ->
      update_acc lexbuf;
      LESSER_EQUAL_DURATION
  | "<^" ->
      update_acc lexbuf;
      LESSER_DURATION
  | ">=^" ->
      update_acc lexbuf;
      GREATER_EQUAL_DURATION
  | ">^" ->
      update_acc lexbuf;
      GREATER_DURATION
  | "+^" ->
      update_acc lexbuf;
      PLUSDURATION
  | "-^" ->
      update_acc lexbuf;
      MINUSDURATION
  | "<=", 0x24 ->
      update_acc lexbuf;
      LESSER_EQUAL_MONEY
  | '<', 0x24 ->
      update_acc lexbuf;
      LESSER_MONEY
  | ">=", 0x24 ->
      update_acc lexbuf;
      GREATER_EQUAL_MONEY
  | '>', 0x24 ->
      update_acc lexbuf;
      GREATER_MONEY
  | '+', 0x24 ->
      update_acc lexbuf;
      PLUSMONEY
  | '-', 0x24 ->
      update_acc lexbuf;
      MINUSMONEY
  | '*', 0x24 ->
      update_acc lexbuf;
      MULTMONEY
  | '/', 0x24 ->
      update_acc lexbuf;
      DIVMONEY
  | "<=." ->
      update_acc lexbuf;
      LESSER_EQUAL_DEC
  | "<." ->
      update_acc lexbuf;
      LESSER_DEC
  | ">=." ->
      update_acc lexbuf;
      GREATER_EQUAL_DEC
  | ">." ->
      update_acc lexbuf;
      GREATER_DEC
  | "+." ->
      update_acc lexbuf;
      PLUSDEC
  | "-." ->
      update_acc lexbuf;
      MINUSDEC
  | "*." ->
      update_acc lexbuf;
      MULTDEC
  | "/." ->
      update_acc lexbuf;
      DIVDEC
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
  | '+' ->
      update_acc lexbuf;
      PLUS
  | '-' ->
      update_acc lexbuf;
      MINUS
  | '*' ->
      update_acc lexbuf;
      MULT
  | '/' ->
      update_acc lexbuf;
      DIV
  | "!=" ->
      update_acc lexbuf;
      NOT_EQUAL
  | '=' ->
      update_acc lexbuf;
      EQUAL
  | '%' ->
      update_acc lexbuf;
      PERCENT
  | '(' ->
      update_acc lexbuf;
      LPAREN
  | ')' ->
      update_acc lexbuf;
      RPAREN
  | '{' ->
      update_acc lexbuf;
      LBRACKET
  | '}' ->
      update_acc lexbuf;
      RBRACKET
  | '[' ->
      update_acc lexbuf;
      LSQUARE
  | ']' ->
      update_acc lexbuf;
      RSQUARE
  | '|' ->
      update_acc lexbuf;
      VERTICAL
  | ':' ->
      update_acc lexbuf;
      COLON
  | ';' ->
      update_acc lexbuf;
      SEMICOLON
  | "--" ->
      update_acc lexbuf;
      ALT
  | '.' ->
      update_acc lexbuf;
      DOT
  | uppercase, Star (uppercase | lowercase | digit | '_' | '\'') ->
      (* Name of constructor *)
      update_acc lexbuf;
      CONSTRUCTOR (Utf8.lexeme lexbuf)
  | lowercase, Star (lowercase | uppercase | digit | '_' | '\'') ->
      (* Name of variable *)
      update_acc lexbuf;
      IDENT (Utf8.lexeme lexbuf)
  | Plus digit ->
      (* Integer literal*)
      update_acc lexbuf;
      INT_LITERAL (Runtime.integer_of_string (Utf8.lexeme lexbuf))
  | _ -> raise_lexer_error (Pos.from_lpos prev_pos) prev_lexeme

(** Main lexing function used outside code blocks *)
let lex_law (lexbuf : lexbuf) : token =
  let prev_lexeme = Utf8.lexeme lexbuf in
  let prev_pos = lexing_positions lexbuf in
  match%sedlex lexbuf with
  | "```catala" ->
      is_code := true;
      code_string_acc := "";
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
  | Plus '#', Star white_space, Plus (Compl '\n'), Star white_space, '\n' -> get_law_heading lexbuf
  | Plus (Compl ('#' | '`' | '>') | Rep ('`', 1 .. 2), Compl '`' | "```", (Plus white_space | '\n'))
    ->
      LAW_TEXT (Utf8.lexeme lexbuf)
  | _ -> raise_lexer_error (Pos.from_lpos prev_pos) prev_lexeme

(** Entry point of the lexer, distributes to {!val: lex_code} or {!val: lex_law} depending of {!val:
    is_code}. *)
let lexer (lexbuf : lexbuf) : token = if !is_code then lex_code lexbuf else lex_law lexbuf

module type LocalisedLexer = sig
  val token_list : (string * Tokens.token) list
  (** Same as {!val: token_list_language_agnostic}, but with tokens specialized to a given language. *)

  val builtins : (string * Ast.builtin_expression) list
  (** Associative list of string to their corresponding builtins *)

  val lex_code : Sedlexing.lexbuf -> Tokens.token
  (** Main lexing function used in code blocks *)

  val lex_law : Sedlexing.lexbuf -> Tokens.token
  (** Main lexing function used outside code blocks *)

  val lexer : Sedlexing.lexbuf -> Tokens.token
  (** Entry point of the lexer, distributes to {!val: lex_code} or {!val: lex_law} depending of
      {!val: Surface.Lexer.is_code}. *)
end
