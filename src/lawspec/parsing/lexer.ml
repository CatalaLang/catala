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
  | "*/" ->
    update lexbuf;
    is_code:= false;
    END_CODE !code_string_acc
  | "choix" -> update_and_acc lexbuf; CHOICE
  | "situation" -> update_and_acc lexbuf; SITUATION
  | "source" -> update_and_acc lexbuf; SOURCE
  | "donnee" -> update_and_acc lexbuf; DATA (* TODO: Find the unicode point of é to enable donnée *)
  | "de" -> update_and_acc lexbuf; OF
  | "type" -> update_and_acc lexbuf; TYPE
  | "collection" -> update_and_acc lexbuf; COLLECTION
  | "entier" -> update_and_acc lexbuf; INTEGER
  | "defini" -> update_and_acc lexbuf; DEFINED
  | "comme" -> update_and_acc lexbuf; AS
  | "condition" -> update_and_acc lexbuf; CONDITION
  | "consequence" -> update_and_acc lexbuf; CONSEQUENCE
  | "regle" -> update_and_acc lexbuf; RULE
  | "existe" -> update_and_acc lexbuf; EXISTS
  | "dans" -> update_and_acc lexbuf; IN
  | "tel" -> update_and_acc lexbuf; SUCH
  | "que" -> update_and_acc lexbuf; THAT
  | "maintenant" -> update_and_acc lexbuf; NOW
  | '!' -> update_and_acc lexbuf; BANG
  | '<' -> update_and_acc lexbuf; LESSER
  | ';' -> update_and_acc lexbuf; SEMICOLON
  | ':' -> update_and_acc lexbuf; COLON
  | '.' -> update_and_acc lexbuf; DOT
  | "--" -> update_and_acc lexbuf; ALT
  | uppercase , Star (uppercase | lowercase) ->
    update_and_acc lexbuf; CONSTRUCTOR (Sedlexing.Utf8.lexeme buf)
  | lowercase , Star (lowercase | '_' | '\'') ->
    update_and_acc lexbuf; IDENT (Sedlexing.Utf8.lexeme buf)
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
