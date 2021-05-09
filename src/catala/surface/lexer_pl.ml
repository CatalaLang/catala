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

open Tokens
open Sedlexing
open Utils
open Lexer_common
module L = Lexer
module R = Re.Pcre

(** Same as {!val: Surface.Lexer.token_list_language_agnostic}, but with tokens specialized to
    Polish. *)
let token_list : (string * token) list =
  [
    ("zakres", SCOPE);
    ("konsekwencja", CONSEQUENCE);
    ("data", DATA);
    ("zalezy od", DEPENDS);
    ("deklaracja", DECLARATION);
    ("kontekst", CONTEXT);
    ("malejacy", DECREASING);
    ("rosnacy", INCREASING);
    ("z", OF);
    ("kolekcja", COLLECTION);
    ("enumeracja", ENUM);
    ("calkowita", INTEGER);
    ("pieniadze", MONEY);
    ("tekst", TEXT);
    ("dziesietna", DECIMAL);
    ("czas", DATE);
    ("czas trwania", DURATION);
    ("zerojedynkowy", BOOLEAN);
    ("suma", SUM);
    ("spelnione", FILLED);
    ("definicja", DEFINITION);
    ("etykieta", LABEL);
    ("wyjatek", EXCEPTION);
    ("wynosi", DEFINED_AS);
    ("pasuje", MATCH);
    ("ze wzorem", WITH);
    ("pod warunkiem", UNDER_CONDITION);
    ("jezeli", IF);
    ("wtedy", THEN);
    ("inaczej", ELSE);
    ("zawartosc", CONTENT);
    ("struktura", STRUCT);
    ("asercja", ASSERTION);
    ("rozna", VARIES);
    ("with", WITH_V);
    ("dla", FOR);
    ("wszystkie", ALL);
    ("mamy", WE_HAVE);
    ("staloprzecinkowa", FIXED);
    ("przez", BY);
    ("zasada", RULE);
    ("istnieje", EXISTS);
    ("takie ze", SUCH);
    ("to", THAT);
    ("i", AND);
    ("lub", OR);
    ("xor", XOR);
    ("nie", NOT);
    ("maximum", MAXIMUM);
    ("minimum", MINIMUM);
    ("filtr", FILTER);
    ("mapuj", MAP);
    ("poczatkowy", INIT);
    ("liczba", CARDINAL);
    ("rok", YEAR);
    ("miesiac", MONTH);
    ("dzien", DAY);
    ("prawda", TRUE);
    ("falsz", FALSE);
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
  | "zakres" ->
      L.update_acc lexbuf;
      SCOPE
  | "data" ->
      L.update_acc lexbuf;
      DATA
  | "zalezy od" ->
      L.update_acc lexbuf;
      DEPENDS
  | "deklaracja" ->
      L.update_acc lexbuf;
      DECLARATION
  | "kontekst" ->
      L.update_acc lexbuf;
      CONTEXT
  | "malejacy" ->
      L.update_acc lexbuf;
      DECREASING
  | "rosnacy" ->
      L.update_acc lexbuf;
      INCREASING
  | "z" ->
      L.update_acc lexbuf;
      OF
  | "kolekcja" ->
      L.update_acc lexbuf;
      COLLECTION
  | "enumeracja" ->
      L.update_acc lexbuf;
      ENUM
  | "calkowita" ->
      L.update_acc lexbuf;
      INTEGER
  | "pieniadze" ->
      L.update_acc lexbuf;
      MONEY
  | "tekst" ->
      L.update_acc lexbuf;
      TEXT
  | "dziesietna" ->
      L.update_acc lexbuf;
      DECIMAL
  | "czas" ->
      L.update_acc lexbuf;
      DATE
  | "czas trwania" ->
      L.update_acc lexbuf;
      DURATION
  | "zerojedynkowy" ->
      L.update_acc lexbuf;
      BOOLEAN
  | "suma" ->
      L.update_acc lexbuf;
      SUM
  | "spelnione" ->
      L.update_acc lexbuf;
      FILLED
  | "definicja" ->
      L.update_acc lexbuf;
      DEFINITION
  | "etykieta" ->
      L.update_acc lexbuf;
      LABEL
  | "wyjatek" ->
      L.update_acc lexbuf;
      EXCEPTION
  | "wynosi" ->
      L.update_acc lexbuf;
      DEFINED_AS
  | "pasuje" ->
      L.update_acc lexbuf;
      MATCH
  | "ze wzorem" ->
      L.update_acc lexbuf;
      WITH
  | "pod warunkiem" ->
      L.update_acc lexbuf;
      UNDER_CONDITION
  | "jezeli" ->
      L.update_acc lexbuf;
      IF
  | "konsekwencja" ->
      L.update_acc lexbuf;
      CONSEQUENCE
  | "wtedy" ->
      L.update_acc lexbuf;
      THEN
  | "inaczej" ->
      L.update_acc lexbuf;
      ELSE
  | "condition" ->
      L.update_acc lexbuf;
      CONDITION
  | "zawartosc" ->
      L.update_acc lexbuf;
      CONTENT
  | "struktura" ->
      L.update_acc lexbuf;
      STRUCT
  | "asercja" ->
      L.update_acc lexbuf;
      ASSERTION
  | "rozna" ->
      L.update_acc lexbuf;
      VARIES
  | "wraz z" ->
      L.update_acc lexbuf;
      WITH_V
  | "dla" ->
      L.update_acc lexbuf;
      FOR
  | "wszystkie" ->
      L.update_acc lexbuf;
      ALL
  | "mamy" ->
      L.update_acc lexbuf;
      WE_HAVE
  | "staloprzecinkowa" ->
      L.update_acc lexbuf;
      FIXED
  | "przez" ->
      L.update_acc lexbuf;
      BY
  | "zasada" ->
      (* 0xE8 is è *)
      L.update_acc lexbuf;
      RULE
  | "istnieje" ->
      L.update_acc lexbuf;
      EXISTS
  | "in" ->
      L.update_acc lexbuf;
      IN
  | "takie ze" ->
      L.update_acc lexbuf;
      SUCH
  | "to" ->
      L.update_acc lexbuf;
      THAT
  | "i" ->
      L.update_acc lexbuf;
      AND
  | "lub" ->
      L.update_acc lexbuf;
      OR
  | "xor" ->
      L.update_acc lexbuf;
      XOR
  | "nie" ->
      L.update_acc lexbuf;
      NOT
  | "maximum" ->
      L.update_acc lexbuf;
      MAXIMUM
  | "minimum" ->
      L.update_acc lexbuf;
      MINIMUM
  | "filtr" ->
      L.update_acc lexbuf;
      FILTER
  | "mapuj" ->
      L.update_acc lexbuf;
      MAP
  | "poczatkowy" ->
      L.update_acc lexbuf;
      INIT
  | "liczba" ->
      L.update_acc lexbuf;
      CARDINAL
  | "prawda" ->
      L.update_acc lexbuf;
      TRUE
  | "falsz" ->
      L.update_acc lexbuf;
      FALSE
  | "rok" ->
      L.update_acc lexbuf;
      YEAR
  | "miesiac" ->
      L.update_acc lexbuf;
      MONTH
  | "dzien" ->
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
      let units =
        Runtime.integer_of_string (R.substitute ~rex:remove_commas ~subst:(fun _ -> "") units)
      in
      let cents =
        try Runtime.integer_of_string (parts 4) with Not_found -> Runtime.integer_of_int 0
      in
      L.update_acc lexbuf;
      MONEY_AMOUNT (units, cents)
  | Plus '0' .. '9', '.', Star '0' .. '9' ->
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
      INT_LITERAL (Runtime.integer_of_string (Utf8.lexeme lexbuf))
  | _ -> L.raise_lexer_error (Pos.from_lpos prev_pos) prev_lexeme

(** Main lexing function used outside code blocks *)
let lex_law (lexbuf : lexbuf) : token =
  let prev_lexeme = Utf8.lexeme lexbuf in
  let prev_pos = lexing_positions lexbuf in
  match%sedlex lexbuf with
  | "```catala" ->
      L.is_code := true;
      L.code_string_acc := "";

      BEGIN_CODE
  | eof -> EOF
  | '#', Star white_space, "Master file" -> MASTER_FILE
  | '>', Star white_space, "Begin metadata" -> BEGIN_METADATA
  | '>', Star white_space, "End metadata" -> END_METADATA
  | ( '>',
      Star white_space,
      "Include:",
      Star white_space,
      Plus (Compl ('@' | '\n')),
      Star white_space,
      Opt ('@', Star white_space, "p.", Star white_space, Plus '0' .. '9', Star white_space),
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
  | '#', Plus '#', Star white_space, Plus (Compl ('[' | ']' | '\n')), Star white_space, '\n' ->
      get_law_heading lexbuf
  | ( '#',
      Plus '#',
      Star white_space,
      '[',
      Star white_space,
      Plus (Compl ']'),
      Star white_space,
      ']',
      '\n' ) ->
      let extract_article_title = R.regexp "([#]+)\\s*\\[([^\\]]+)\\]" in
      let get_substring =
        R.get_substring (R.exec ~rex:extract_article_title (Utf8.lexeme lexbuf))
      in
      let title = get_substring 2 in
      let get_new_lines = R.regexp "\n" in
      let new_lines_count =
        try Array.length (R.extract ~rex:get_new_lines (Utf8.lexeme lexbuf)) with Not_found -> 0
      in
      (* the -1 is here to compensate for Sedlex's automatic newline detection around token *)
      for _i = 1 to new_lines_count - 1 do
        new_line lexbuf
      done;
      let precedence = calc_precedence (get_substring 1) in

      LAW_ARTICLE (title, None, None, precedence)
  | Plus (Compl ('/' | '#' | '`' | '>')) -> LAW_TEXT (Utf8.lexeme lexbuf)
  | _ -> L.raise_lexer_error (Pos.from_lpos prev_pos) prev_lexeme

(** Entry point of the lexer, distributes to {!val: lex_code} or {!val: lex_law} depending of {!val:
    Surface.Lexer.is_code}. *)
let lexer (lexbuf : lexbuf) : token = if !L.is_code then lex_code lexbuf else lex_law lexbuf
