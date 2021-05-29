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
    to French. *)
let token_list : (string * token) list =
  [
    ("champ d'application", SCOPE);
    ("conséquence", CONSEQUENCE);
    ("donnée", DATA);
    ("dépend de", DEPENDS);
    ("déclaration", DECLARATION);
    ("contexte", CONTEXT);
    ("décroissant", DECREASING);
    ("croissant", INCREASING);
    ("de", OF);
    ("collection", COLLECTION);
    ("énumération", ENUM);
    ("entier", INTEGER);
    ("argent", MONEY);
    ("texte", TEXT);
    ("decimal", DECIMAL);
    ("date", DATE);
    ("durée", DURATION);
    ("booléen", BOOLEAN);
    ("somme", SUM);
    ("rempli", FILLED);
    ("définition", DEFINITION);
    ("égal à", DEFINED_AS);
    ("selon", MATCH);
    ("sous forme", WITH);
    ("sous condition", UNDER_CONDITION);
    ("si", IF);
    ("alors", THEN);
    ("sinon", ELSE);
    ("contenu", CONTENT);
    ("structure", STRUCT);
    ("assertion", ASSERTION);
    ("varie", VARIES);
    ("avec", WITH_V);
    ("pour", FOR);
    ("tout", ALL);
    ("on a", WE_HAVE);
    ("fixé", FIXED);
    ("par", BY);
    ("règle", RULE);
    ("existe", EXISTS);
    ("tel", SUCH);
    ("que", THAT);
    ("et", AND);
    ("ou", OR);
    ("ou bien", XOR);
    ("non", NOT);
    ("nombre", CARDINAL);
    ("maximum", MAXIMUM);
    ("minimum", MINIMUM);
    ("filtre", FILTER);
    ("application", MAP);
    ("initial", INIT);
    ("an", YEAR);
    ("mois", MONTH);
    ("jour", DAY);
    ("vrai", TRUE);
    ("faux", FALSE);
  ]
  @ L.token_list_language_agnostic

let builtins : (string * Ast.builtin_expression) list =
  [
    ("entier_vers_décimal", Ast.IntToDec);
    ("accès_jour", Ast.GetDay);
    ("accès_mois", Ast.GetMonth);
    ("accès_année", Ast.GetYear);
  ]

(** Regexp matching any digit character.

    @note can not be used outside the current module (@see <
    https://github.com/ocaml-community/sedlex#lexer-specifications >). *)
let digit = [%sedlex.regexp? '0' .. '9']

(** Main lexing function used in code blocks *)
let rec lex_code (lexbuf : lexbuf) : token =
  let prev_lexeme = Utf8.lexeme lexbuf in
  let prev_pos = lexing_positions lexbuf in
  match%sedlex lexbuf with
  | white_space | '\n' ->
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
  | "champ d\'application" ->
      L.update_acc lexbuf;
      SCOPE
  | "donn", 0xE9, "e" ->
      (* 0xE9 is é *)
      L.update_acc lexbuf;
      DATA
  | "d", 0xE9, "pend de" ->
      L.update_acc lexbuf;
      DEPENDS
  | "d", 0xE9, "claration" ->
      L.update_acc lexbuf;
      DECLARATION
  | "contexte" ->
      L.update_acc lexbuf;
      CONTEXT
  | "d", 0xE9, "croissant" ->
      L.update_acc lexbuf;
      DECREASING
  | "croissant" ->
      L.update_acc lexbuf;
      INCREASING
  | "de" ->
      L.update_acc lexbuf;
      OF
  | "collection" ->
      L.update_acc lexbuf;
      COLLECTION
  | 0xE9, "num", 0xE9, "ration" ->
      L.update_acc lexbuf;
      ENUM
  | "entier" ->
      L.update_acc lexbuf;
      INTEGER
  | "argent" ->
      L.update_acc lexbuf;
      MONEY
  | "texte" ->
      L.update_acc lexbuf;
      TEXT
  | "d", 0xE9, "cimal" ->
      L.update_acc lexbuf;
      DECIMAL
  | "date" ->
      L.update_acc lexbuf;
      DATE
  | "dur", 0xE9, "e" ->
      L.update_acc lexbuf;
      DURATION
  | "bool", 0xE9, "en" ->
      L.update_acc lexbuf;
      BOOLEAN
  | "somme" ->
      L.update_acc lexbuf;
      SUM
  | "rempli" ->
      L.update_acc lexbuf;
      FILLED
  | "d", 0xE9, "finition" ->
      (* 0xE9 is é *)
      L.update_acc lexbuf;
      DEFINITION
  | 0xE9, "tiquette" ->
      L.update_acc lexbuf;
      LABEL
  | "exception" ->
      L.update_acc lexbuf;
      EXCEPTION
  | 0xE9, "gal ", 0x00E0 ->
      (* 0xE9 is é *)
      L.update_acc lexbuf;
      DEFINED_AS
  | "selon" ->
      L.update_acc lexbuf;
      MATCH
  | "sous forme" ->
      L.update_acc lexbuf;
      WITH
  | "sous condition" ->
      L.update_acc lexbuf;
      UNDER_CONDITION
  | "si" ->
      L.update_acc lexbuf;
      IF
  | "cons", 0xE9, "quence" ->
      (* 0xE9 is é *)
      L.update_acc lexbuf;
      CONSEQUENCE
  | "alors" ->
      (* 0xE9 is é *)
      L.update_acc lexbuf;
      THEN
  | "sinon" ->
      L.update_acc lexbuf;
      ELSE
  | "condition" ->
      L.update_acc lexbuf;
      CONDITION
  | "contenu" ->
      L.update_acc lexbuf;
      CONTENT
  | "structure" ->
      L.update_acc lexbuf;
      STRUCT
  | "assertion" ->
      L.update_acc lexbuf;
      ASSERTION
  | "varie" ->
      L.update_acc lexbuf;
      VARIES
  | "avec" ->
      L.update_acc lexbuf;
      WITH_V
  | "pour" ->
      L.update_acc lexbuf;
      FOR
  | "tout" ->
      L.update_acc lexbuf;
      ALL
  | "on a" ->
      L.update_acc lexbuf;
      WE_HAVE
  | "fix", 0xE9 ->
      (* 0xE9 is é *)
      L.update_acc lexbuf;
      FIXED
  | "par" ->
      L.update_acc lexbuf;
      BY
  | "r", 0xE8, "gle" ->
      (* 0xE8 is è *)
      L.update_acc lexbuf;
      RULE
  | "existe" ->
      L.update_acc lexbuf;
      EXISTS
  | "dans" ->
      L.update_acc lexbuf;
      IN
  | "tel" ->
      L.update_acc lexbuf;
      SUCH
  | "que" ->
      L.update_acc lexbuf;
      THAT
  | "et" ->
      L.update_acc lexbuf;
      AND
  | "ou" ->
      L.update_acc lexbuf;
      OR
  | "ou bien" ->
      L.update_acc lexbuf;
      XOR
  | "non" ->
      L.update_acc lexbuf;
      NOT
  | "nombre" ->
      L.update_acc lexbuf;
      CARDINAL
  | "maximum" ->
      L.update_acc lexbuf;
      MAXIMUM
  | "minimum" ->
      L.update_acc lexbuf;
      MINIMUM
  | "filtre" ->
      L.update_acc lexbuf;
      FILTER
  | "application" ->
      L.update_acc lexbuf;
      MAP
  | "initial" ->
      L.update_acc lexbuf;
      INIT
  | "vrai" ->
      L.update_acc lexbuf;
      TRUE
  | "faux" ->
      L.update_acc lexbuf;
      FALSE
  | "an" ->
      L.update_acc lexbuf;
      YEAR
  | "mois" ->
      L.update_acc lexbuf;
      MONTH
  | "jour" ->
      L.update_acc lexbuf;
      DAY
  | digit, Star (digit | white_space), Opt (',', Rep (digit, 0 .. 2)), Star white_space, 0x20AC ->
      let extract_parts = R.regexp "([0-9]([0-9 ]*[0-9]|))(,([0-9]{0,2})|)" in
      let full_str = Utf8.lexeme lexbuf in
      let only_numbers_str = String.trim (String.sub full_str 0 (String.length full_str - 1)) in
      let parts = R.get_substring (R.exec ~rex:extract_parts only_numbers_str) in
      (* Integer literal*)
      let units = parts 1 in
      let remove_spaces = R.regexp " " in
      let units =
        Runtime.integer_of_string (R.substitute ~rex:remove_spaces ~subst:(fun _ -> "") units)
      in
      let cents =
        try Runtime.integer_of_string (parts 4) with Not_found -> Runtime.integer_of_int 0
      in
      L.update_acc lexbuf;
      MONEY_AMOUNT (units, cents)
  | Plus digit, ',', Star digit ->
      let extract_code_title = R.regexp "([0-9]+),([0-9]*)" in
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
  | "<=", 0x20AC ->
      L.update_acc lexbuf;
      LESSER_EQUAL_MONEY
  | '<', 0x20AC ->
      L.update_acc lexbuf;
      LESSER_MONEY
  | ">=", 0x20AC ->
      L.update_acc lexbuf;
      GREATER_EQUAL_MONEY
  | '>', 0x20AC ->
      L.update_acc lexbuf;
      GREATER_MONEY
  | '+', 0x20AC ->
      L.update_acc lexbuf;
      PLUSMONEY
  | '-', 0x20AC ->
      L.update_acc lexbuf;
      MINUSMONEY
  | '*', 0x20AC ->
      L.update_acc lexbuf;
      MULTMONEY
  | '/', 0x20AC ->
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
  | '>', Star white_space, 'D', 0xE9, "but m", 0xE9, "tadonn", 0xE9, "es" -> BEGIN_METADATA
  | '>', Star white_space, "Fin m", 0xE9, "tadonn", 0xE9, "es" -> END_METADATA
  | ( '>',
      Star white_space,
      "Inclusion:",
      Star white_space,
      Plus (Compl ('@' | '\n')),
      Star white_space,
      Opt ('@', Star white_space, "p.", Star white_space, Plus digit, Star white_space),
      '\n' ) ->
      let extract_components =
        R.regexp ">\\s*Inclusion\\:\\s*([^@\\n]+)\\s*(@\\s*p\\.\\s*([0-9]+)|)"
      in
      let get_component = R.get_substring (R.exec ~rex:extract_components (Utf8.lexeme lexbuf)) in
      let jorftext = R.regexp "JORFTEXT\\d{12}" in
      let name = get_component 1 in
      let pages = try Some (int_of_string (get_component 3)) with Not_found -> None in
      let pos = lexing_positions lexbuf in
      if R.pmatch ~rex:jorftext name then
        LAW_INCLUDE (Ast.LegislativeText (name, Pos.from_lpos pos))
      else if Filename.extension name = ".pdf" then
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
