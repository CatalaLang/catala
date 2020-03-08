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

let rec lex_code lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '\n' -> update lexbuf ; new_line lexbuf; lex_code lexbuf
  | ' ' | '\t' -> update lexbuf; lex_code lexbuf
  | "*/" -> update lexbuf; is_code:= false; lex_law lexbuf
  | "code" -> update lexbuf; CODE
  | any -> update lexbuf; lex_code lexbuf
  | _ -> raise_ParseError lexbuf

and lex_law lexbuf =
  let buf = lexbuf.stream in
  match%sedlex buf with
  | '\n' -> update lexbuf ; new_line lexbuf; lex_law lexbuf
  | "/*" -> update lexbuf; is_code := true; lex_code lexbuf
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
