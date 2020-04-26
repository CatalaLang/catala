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

(** Error formatting and helper functions *)

(**{1 Parsing}*)

exception ParsingError of string

exception LexingError of string

let parser_error (loc : Lexing.position * Lexing.position) (token : string) (msg : string) =
  raise
    (ParsingError
       (Printf.sprintf "Syntax error at token \"%s\" %s\n%s" token (Pos.to_string loc) msg))

let lexer_error (loc : Lexing.position * Lexing.position) (msg : string) =
  raise (LexingError (Printf.sprintf "Parsing error %s on token \"%s\"" (Pos.to_string loc) msg))
