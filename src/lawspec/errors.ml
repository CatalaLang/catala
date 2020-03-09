(* This file is part of the Lawspec compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2019 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Error formatting and helper functions *)

(**{1 Parsing}*)

exception ParsingError of string

exception LexingError of string

let print_lexer_position (pos : Lexing.position) : string =
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let lexer_error lexbuf =
  Printf.sprintf "Incorrect character (%s) at position %s in file %s" (Lexing.lexeme lexbuf)
    (print_lexer_position lexbuf.Lexing.lex_curr_p)
    lexbuf.Lexing.lex_curr_p.Lexing.pos_fname

let parser_error (sloc_start, sloc_end) (msg : string) =
  raise
    (ParsingError
       (Printf.sprintf "Parsing error: %s (file %s, %s to %s)" msg sloc_start.Lexing.pos_fname
          (print_lexer_position sloc_start) (print_lexer_position sloc_end)))
