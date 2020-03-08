(*
  This file is part of the Lawspec compiler, a specification language for tax and social benefits
  computation rules.
  Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

open Parser

open Sedlex_menhir

module R = Re.Pcre

let is_code : bool ref = ref false
let code_string_acc : string ref = ref ""

let rec lex_code_as_string lexbuf acc =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | "*/" -> update lexbuf; END_CODE (acc ^ (Sedlexing.Utf8.lexeme buf))
  | any -> update lexbuf; lex_code_as_string lexbuf (acc ^ (Sedlexing.Utf8.lexeme buf))
  | _ -> raise_ParseError lexbuf

let update_and_acc lexbuf =
  update lexbuf;
  code_string_acc := !code_string_acc ^ (Sedlexing.Utf8.lexeme lexbuf.stream)

let rec lex_code lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '\n' -> update_and_acc lexbuf ; new_line lexbuf; lex_code lexbuf
  | white_space -> update_and_acc lexbuf; lex_code lexbuf
  | '#', Star (Compl '\n'), '\n' ->
    update_and_acc lexbuf; new_line lexbuf; lex_code lexbuf
  | "*/" ->
    update lexbuf;
    is_code:= false;
    END_CODE !code_string_acc
  | "choix" -> update_and_acc lexbuf; CHOICE
  | "situation" -> update_and_acc lexbuf; SITUATION
  | "source" -> update_and_acc lexbuf; SOURCE
  | "donnee" -> update_and_acc lexbuf; DATA (* TODO: Find the unicode point of é to enable donnée *)
  | "decroissant" -> update_and_acc lexbuf; DECREASING
  | "croissant" -> update_and_acc lexbuf; INCREASING
  | "de" -> update_and_acc lexbuf; OF
  | "type" -> update_and_acc lexbuf; TYPE
  | "collection" -> update_and_acc lexbuf; COLLECTION
  | "fonction" -> update_and_acc lexbuf; FUNCTION
  | "parametres" -> update_and_acc lexbuf; PARAMETERS
  | "renvoie" -> update_and_acc lexbuf; RETURNS
  | "entier" -> update_and_acc lexbuf; INTEGER
  | "defini" -> update_and_acc lexbuf; DEFINED
  | "comme" -> update_and_acc lexbuf; AS
  | "selon" -> update_and_acc lexbuf; MATCH
  | "sous forme" -> update_and_acc lexbuf; WITH
  | "condition" -> update_and_acc lexbuf; CONDITION
  | "consequence" -> update_and_acc lexbuf; CONSEQUENCE
  | "constante" -> update_and_acc lexbuf; CONSTANT
  | "optionnel" -> update_and_acc lexbuf; OPTIONAL
  | "assertion" -> update_and_acc lexbuf; ASSERTION
  | "varie avec" -> update_and_acc lexbuf; VARIES_WITH
  | "pour tout" -> update_and_acc lexbuf; FORALL
  | "on a" -> update_and_acc lexbuf; WE_HAVE
  | "fixe" -> update_and_acc lexbuf; FIXED
  | "par" -> update_and_acc lexbuf; BY
  | "regle" -> update_and_acc lexbuf; RULE
  | "existe" -> update_and_acc lexbuf; EXISTS
  | "dans" -> update_and_acc lexbuf; IN
  | "tel" -> update_and_acc lexbuf; SUCH
  | "que" -> update_and_acc lexbuf; THAT
  | "maintenant" -> update_and_acc lexbuf; NOW
  | "et" -> update_and_acc lexbuf; AND
  | "ou" -> update_and_acc lexbuf; OR
  | "non" -> update_and_acc lexbuf; NOT
  | "cardinal" -> update_and_acc lexbuf; CARDINAL
  | "an" -> update_and_acc lexbuf; YEAR
  | '!' -> update_and_acc lexbuf; BANG
  | "<=" -> update_and_acc lexbuf; LESSER_EQUAL
  | '<' -> update_and_acc lexbuf; LESSER
  | ">=" -> update_and_acc lexbuf; GREATER_EQUAL
  | '>' -> update_and_acc lexbuf; GREATER
  | '=' -> update_and_acc lexbuf; EQUAL
  | '(' -> update_and_acc lexbuf; LPAREN
  | ')' -> update_and_acc lexbuf; RPAREN
  | '+' -> update_and_acc lexbuf; PLUS
  | '-' -> update_and_acc lexbuf; MINUS
  | '*' -> update_and_acc lexbuf; MULT
  | '/' -> update_and_acc lexbuf; DIV
  | ',' -> update_and_acc lexbuf; COMMA
  | ';' -> update_and_acc lexbuf; SEMICOLON
  | ':' -> update_and_acc lexbuf; COLON
  | '.' -> update_and_acc lexbuf; DOT
  | "--" -> update_and_acc lexbuf; ALT
  | uppercase , Star (uppercase | lowercase | '0' .. '9') ->
    update_and_acc lexbuf; CONSTRUCTOR (Sedlexing.Utf8.lexeme buf)
  | lowercase , Star (lowercase | uppercase | '0' .. '9' | '_' | '\'') ->
    update_and_acc lexbuf; IDENT (Sedlexing.Utf8.lexeme buf)
  | Plus ('0' .. '9') ->
    update_and_acc lexbuf; INT_LITERAL (int_of_string (Sedlexing.Utf8.lexeme buf))
  | _ -> raise_ParseError lexbuf

let rec lex_law lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '\n' -> update lexbuf ; new_line lexbuf; lex_law lexbuf
  | "/*" ->
    update lexbuf;
    is_code := true;
    code_string_acc := "";
    BEGIN_CODE
  | eof -> update lexbuf ; EOF
  | "@@", Star white_space, Star (Compl '@'), Star white_space, "@@" ->
   let extract_code_title =
    R.regexp "@@\\s*([^#]*)\\s*@@"
   in
   let title = R.get_substring
    (R.exec ~rex:extract_code_title (Sedlexing.Utf8.lexeme buf))
    1
   in
   update lexbuf; LAW_CODE title
  | "@", Star white_space, Star (Compl '@'), Star white_space, "@" ->
   let extract_article_title =
    R.regexp "@\\s*([^#]*)\\s*@"
   in
   let title = R.get_substring
    (R.exec ~rex:extract_article_title (Sedlexing.Utf8.lexeme buf))
    1
   in
   update lexbuf; LAW_ARTICLE title
  | Plus (Compl ('@' | '/' | '\n'))  ->
    update lexbuf; LAW_TEXT (Sedlexing.Utf8.lexeme buf)
  | _ -> raise_ParseError lexbuf

let lexer lexbuf =
  if !is_code then lex_code lexbuf else lex_law lexbuf
