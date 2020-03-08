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

open Cli

(** Entry function for the executable. Returns a negative number in case of error. *)
let driver
  (source_files : string list)
  (debug: bool)
  (_backend: string)
  (_output_file : string)
  : int =
Cli.debug_flag := debug;
  Cli.debug_print "Reading files...";
  let program = ref [] in
  List.iter (fun source_file ->
      let input = open_in source_file in
      Cli.debug_print (Printf.sprintf "Parsing %s" source_file);
      let lexbuf = Sedlex_menhir.create_lexbuf ~file:(Filename.basename source_file) (Sedlexing.Utf8.from_channel input) in
      try
        Parse_utils.current_file := source_file;
        let commands = Sedlex_menhir.sedlex_with_menhir Lexer.lexer Parser.source_file lexbuf in
        program := commands::!program;
        close_in input
      with
      | Errors.LexingError msg | Errors.ParsingError msg ->
        error_print msg
      | Sedlex_menhir.ParseError msg -> begin
          error_print
            (Printf.sprintf "Parser error: %s" msg);
          close_in input;
          exit (-1)
        end
    ) source_files;
  exit 0

let main () =
  Cmdliner.Term.exit @@ Cmdliner.Term.eval (Cli.lawspec_t driver, Cli.info)
