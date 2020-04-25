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
let create_lexbuf ?(file = "") (stream : Sedlexing.lexbuf) : lexbuf =
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
let new_line (lexbuf : lexbuf) : unit =
  let open Lexing in
  let lcp = lexbuf.pos in
  lexbuf.pos <- { lcp with pos_lnum = lcp.pos_lnum + 1; pos_bol = lcp.pos_cnum }

(** Update the position with the stream. *)
let update (lexbuf : lexbuf) : unit =
  let new_pos = Sedlexing.lexeme_end lexbuf.stream in
  let p = lexbuf.pos in
  lexbuf.pos <- { p with Lexing.pos_cnum = new_pos }

(** The last matched word. *)
let lexeme ({ stream; _ } : lexbuf) : string = Sedlexing.Utf8.lexeme stream

let raise_ParseError (lexbuf : lexbuf) : 'a =
  let { pos; _ } = lexbuf in
  let tok = lexeme lexbuf in
  Errors.parser_error pos (Printf.sprintf "unexpected token \"%s\"" tok)

let raise_LexError (lexbuf : lexbuf) : 'a =
  let { pos; _ } = lexbuf in
  let tok = lexeme lexbuf in
  Errors.lexer_error pos (Printf.sprintf "unexpected token \"%s\"" tok)

module I = Parser.MenhirInterpreter

(* let lexbuf_translation (lexbuf: Sedlexing.lexbuf) : Lexing.lexbuf =

   let succeed (v : 'semantic_value) : 'semantic_value = (* The parser has succeeded and produced a
   semantic value. Return it *) v

   let fail (lexbuf: Lexing.lexbuf) (_ : 'semantic_value I.checkpoint) = (* The parser has suspended
   itself because of a syntax error. Stop. *) raise_ParseError lexbuf

   let loop (lexbuf: Lexing.lexbuf) result = let supplier = I.lexer_lexbuf_to_supplie Lexer.token
   lexbuf in I.loop_handle succeed (fail lexbuf) supplier result *)

let sedlex_with_menhir (lexer' : lexbuf -> Parser.token)
    (parser' : (Parser.token, 'semantic_value) MenhirLib.Convert.traditional) (lexbuf : lexbuf) :
    'semantic_value =
  let lexer () : Parser.token * Lexing.position * Lexing.position =
    let ante_position = lexbuf.pos in
    let token = lexer' lexbuf in
    let post_position = lexbuf.pos in
    (token, ante_position, post_position)
  in
  let parser : (Parser.token * Lexing.position * Lexing.position, 'a) MenhirLib.Convert.revised =
    MenhirLib.Convert.Simplified.traditional2revised parser'
  in
  try parser lexer with
  | Parser.Error -> raise_ParseError lexbuf
  | Sedlexing.MalFormed | Sedlexing.InvalidCodepoint _ -> raise_LexError lexbuf
