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

(* Boilerplate for using sedlex with Menhir, based on
   https://github.com/Drup/llvm/blob/3c43000f4e86af5b9b368f50721604957d403750/test/Bindings/OCaml/kaleidoscope/src/syntax.ml *)

type lexbuf = { stream : Sedlexing.lexbuf; mutable pos : Lexing.position }
(** The state of the parser, a stream and a position. *)

(** Initialize with the null position. *)
let create_lexbuf ?(file = "") stream =
  let pos =
    {
      Lexing.pos_fname = file;
      pos_lnum = 1;
      (* Start lines at 1, not 0 *)
      pos_bol = 0;
      pos_cnum = 0;
    }
  in
  { pos; stream }

(** Register a new line in the lexer's position. *)
let new_line lexbuf =
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <- { lcp with pos_lnum = lcp.pos_lnum + 1; pos_bol = lcp.pos_cnum }

(** Update the position with the stream. *)
let update lexbuf =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- { p with Lexing.pos_cnum = new_pos }

(** The last matched word. *)
let lexeme { stream; _ } = Sedlexing.Utf8.lexeme stream

exception ParseError of string

let raise_ParseError lexbuf =
  let { pos; _ } = lexbuf in
  let tok = lexeme lexbuf in
  Errors.parser_error pos (Printf.sprintf "unexpected token \"%s\"" tok)

let sedlex_with_menhir lexer' parser' lexbuf =
  let lexer () =
    let ante_position = lexbuf.pos in
    let token = lexer' lexbuf in
    let post_position = lexbuf.pos in
    (token, ante_position, post_position)
  in
  let parser = MenhirLib.Convert.Simplified.traditional2revised parser' in
  try parser lexer
  with Parser_fr.Error | Sedlexing.MalFormed | Sedlexing.InvalidCodepoint _ ->
    raise_ParseError lexbuf
