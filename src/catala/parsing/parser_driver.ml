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

let rec parse_source_files (source_files : string list) (language : string) : Ast.program =
  match source_files with
  | [] -> { program_items = []; program_source_files = [] }
  | source_file :: rest -> (
      let input = open_in source_file in
      Cli.debug_print (Printf.sprintf "Parsing %s" source_file);
      let lexbuf =
        Sedlex_menhir.create_lexbuf ~file:(Filename.basename source_file)
          (Sedlexing.Utf8.from_channel input)
      in
      try
        Parse_utils.current_file := source_file;
        let lexer_lang =
          if language = "fr" then Lexer_fr.lexer_fr
          else if language = "en" then Lexer_en.lexer_en
          else begin
            Cli.error_print
              (Printf.sprintf "The selected language (%s) is not supported by Catala" language);
            exit (-1)
          end
        in
        let commands_or_includes =
          Sedlex_menhir.sedlex_with_menhir lexer_lang Parser.source_file_or_master lexbuf
        in
        close_in input;
        match commands_or_includes with
        | Ast.SourceFile commands ->
            let rest_program = parse_source_files rest language in
            {
              program_items = commands @ rest_program.Ast.program_items;
              program_source_files = source_file :: rest_program.Ast.program_source_files;
            }
        | Ast.MasterFile includes ->
            let current_source_file_dirname = Filename.dirname source_file in
            let includes =
              List.map
                (fun includ -> current_source_file_dirname ^ "/" ^ Pos.unmark includ)
                includes
            in
            let new_program = parse_source_files (includes @ rest) language in
            {
              new_program with
              program_source_files = source_file :: new_program.program_source_files;
            }
      with Errors.ParsingError msg ->
        Cli.error_print msg;
        close_in input;
        exit (-1) )
