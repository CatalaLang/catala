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

open Sedlexing
module I = Parser.MenhirInterpreter

let state (env : 'semantic_value I.env) : int =
  match Lazy.force (I.stack env) with
  | MenhirLib.General.Nil -> 0
  | MenhirLib.General.Cons (Element (s, _, _, _), _) -> I.number s

let fail (lexbuf : lexbuf) (env : 'semantic_value I.env)
    (_checkpoint : 'semantic_value I.checkpoint) : 'a =
  (* The parser has suspended itself because of a syntax error. Stop. *)
  let message =
    match Parser_errors.message (state env) with
    | exception Not_found -> "Syntax error"
    | msg -> msg
  in
  Errors.parser_error (lexing_positions lexbuf) (Utf8.lexeme lexbuf) message

let rec loop (next_token : unit -> Parser.token * Lexing.position * Lexing.position)
    (lexbuf : lexbuf) (checkpoint : 'semantic_value I.checkpoint) : Ast.source_file_or_master =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = next_token () in
      let checkpoint = I.offer checkpoint token in
      loop next_token lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      loop next_token lexbuf checkpoint
  | I.HandlingError env -> fail lexbuf env checkpoint
  | I.Accepted v -> v
  | I.Rejected ->
      (* Cannot happen as we stop at syntax error immediatly *)
      assert false

let sedlex_with_menhir (lexer' : lexbuf -> Parser.token)
    (target_rule : Lexing.position -> 'semantic_value I.checkpoint) (lexbuf : lexbuf) :
    Ast.source_file_or_master =
  let lexer : unit -> Parser.token * Lexing.position * Lexing.position =
    with_tokenizer lexer' lexbuf
  in
  try loop lexer lexbuf (target_rule (fst @@ Sedlexing.lexing_positions lexbuf))
  with Sedlexing.MalFormed | Sedlexing.InvalidCodepoint _ ->
    Errors.lexer_error (lexing_positions lexbuf) (Utf8.lexeme lexbuf)

let rec parse_source_files (source_files : string list) (language : Cli.language_option) :
    Ast.program =
  match source_files with
  | [] -> { program_items = []; program_source_files = [] }
  | source_file :: rest -> (
      Cli.debug_print (Printf.sprintf "Parsing %s" source_file);
      let input = open_in source_file in
      let lexbuf = Sedlexing.Utf8.from_channel input in
      Sedlexing.set_filename lexbuf source_file;
      try
        Parse_utils.current_file := source_file;
        let lexer_lang =
          match language with Cli.Fr -> Lexer_fr.lexer_fr | Cli.En -> Lexer_en.lexer_en
        in
        let commands_or_includes =
          sedlex_with_menhir lexer_lang Parser.Incremental.source_file_or_master lexbuf
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
                (fun includ ->
                  ( if current_source_file_dirname = "./" then ""
                  else current_source_file_dirname ^ "/" )
                  ^ Pos.unmark includ)
                includes
            in
            let new_program = parse_source_files (includes @ rest) language in
            {
              new_program with
              program_source_files = source_file :: new_program.program_source_files;
            }
      with Errors.ParsingError msg | Errors.LexingError msg ->
        Cli.error_print msg;
        close_in input;
        exit (-1) )
