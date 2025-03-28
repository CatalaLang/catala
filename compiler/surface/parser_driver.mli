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

val lines :
  File.t ->
  Global.backend_lang ->
  (string * Lexer_common.line_token * (Lexing.position * Lexing.position)) Seq.t
(** Raw file parser that doesn't interpret any includes and returns the flat law
    structure as is *)

val load_interface :
  ?default_module_name:string -> File.t Global.input_src -> Ast.module_content
(** Reads only declarations in metadata in the supplied input file, and only
    keeps type information. The list of submodules is initialised with names
    only and empty contents. *)

val load_interface_and_code :
  ?default_module_name:string -> File.t Global.input_src -> Ast.module_content
(** Reads the supplied input file and returns its interface that contains type
    information as well as scope and topdef definitions. The list of submodules
    is initialised with names only and empty contents. *)

val register_included_file_resolver :
  filename:string -> new_content:string Global.input_src -> unit
(** Register an included file dynamic resolver. When the surface's parser
    encounters an inclusion that correspond to the [filename] argument, it will
    dynamically replace it with the given [new_content]. *)

val parse_top_level_file :
  ?resolve_included_file:(string -> string Global.input_src) ->
  File.t Global.input_src ->
  Ast.program
(** Parses a catala file (handling file includes) and returns a program.
    Interfaces of the used modules are returned empty, use [load_interface] to
    fill them. When provided [resolve_included_file] replaces file includes with
    an user provided input source. However, it will shadow any existing dynamic
    resolver registered using [register_included_file_resolver]. *)
