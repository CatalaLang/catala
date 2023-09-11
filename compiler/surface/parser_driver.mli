(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Wrapping module around parser and lexer that offers the
    [Surface.Parser_driver.parse_source_file] API. *)

open Catala_utils

val lines : File.t -> Cli.backend_lang -> (string * Lexer_common.line_token) Seq.t
(** Raw file parser that doesn't interpret any includes and returns the flat law
    structure as is *)

val load_interface : Cli.input_file -> Cli.backend_lang -> Ast.interface
(** Reads only declarations in metadata in the supplied input file, and only
    keeps type information ; returns the declared module name as well *)

val parse_top_level_file : Cli.input_file -> Cli.backend_lang -> Ast.program
(** Parses a catala file (handling file includes) and returns a program. Modules
    in the program are returned empty, use [load_interface] to fill them. *)
