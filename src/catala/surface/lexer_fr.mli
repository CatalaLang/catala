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

(** Same as {!val: Surface.Lexer.token_list_language_agnostic}, but with tokens specialized to
    French. *)
val token_list_fr : (string * Parser.token) list

(** Main lexing function used in code blocks *)
val lex_code_fr : Sedlexing.lexbuf -> Parser.token

(** Main lexing function used outside code blocks *)
val lex_law_fr : Sedlexing.lexbuf -> Parser.token

(** Entry point of the lexer, distributes to {!val: lex_code_fr} or {!val: lex_law_fr} depending of
    {!val: Surface.Lexer.is_code}. *)
val lexer_fr : Sedlexing.lexbuf -> Parser.token
