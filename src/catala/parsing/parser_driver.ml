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

(** Computes the levenshtein distance between two strings *)
let minimum a b c = min a (min b c)

let levenshtein_distance (s : string) (t : string) : int =
  let m = String.length s and n = String.length t in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between the first i characters of
     s and the first j characters of t *)
  let d = Array.make_matrix (m + 1) (n + 1) 0 in

  for i = 0 to m do
    d.(i).(0) <- i (* the distance of any first string to an empty second string *)
  done;
  for j = 0 to n do
    d.(0).(j) <- j (* the distance of any second string to an empty first string *)
  done;

  for j = 1 to n do
    for i = 1 to m do
      if s.[i - 1] = t.[j - 1] then d.(i).(j) <- d.(i - 1).(j - 1) (* no operation required *)
      else
        d.(i).(j) <-
          minimum
            (d.(i - 1).(j) + 1) (* a deletion *)
            (d.(i).(j - 1) + 1) (* an insertion *)
            (d.(i - 1).(j - 1) + 1) (* a substitution *)
    done
  done;

  d.(m).(n)

let fail (lexbuf : lexbuf) (env : 'semantic_value I.env) (token_list : (string * Parser.token) list)
    (last_input_needed : 'semantic_value I.env option) : 'a =
  let wrong_token = Utf8.lexeme lexbuf in
  let acceptable_tokens =
    match last_input_needed with
    | Some last_input_needed ->
        List.filter
          (fun (_, t) ->
            I.acceptable (I.input_needed last_input_needed) t (fst (lexing_positions lexbuf)))
          token_list
    | None -> token_list
  in
  let similar_acceptable_tokens =
    List.sort
      (fun (x, _) (y, _) ->
        let truncated_x =
          if String.length wrong_token <= String.length x then
            String.sub x 0 (String.length wrong_token)
          else x
        in
        let truncated_y =
          if String.length wrong_token <= String.length y then
            String.sub y 0 (String.length wrong_token)
          else y
        in
        let levx = levenshtein_distance truncated_x wrong_token in
        let levy = levenshtein_distance truncated_y wrong_token in
        if levx = levy then String.length x - String.length y else levx - levy)
      acceptable_tokens
  in
  let similar_token_msg =
    if List.length similar_acceptable_tokens = 0 then None
    else
      Some
        (Printf.sprintf "did you mean %s?"
           (String.concat ", or maybe "
              (List.map (fun (ts, _) -> Printf.sprintf "\"%s\"" ts) similar_acceptable_tokens)))
  in
  (* The parser has suspended itself because of a syntax error. Stop. *)
  let custom_menhir_message =
    match Parser_errors.message (state env) with
    | exception Not_found -> "Message: unexpected token"
    | msg -> "Message: " ^ String.trim (String.uncapitalize_ascii msg)
  in
  let msg =
    match similar_token_msg with
    | None -> custom_menhir_message
    | Some similar_token_msg ->
        Printf.sprintf "%s\nAutosuggestion: %s" custom_menhir_message similar_token_msg
  in
  Errors.parser_error (lexing_positions lexbuf) (Utf8.lexeme lexbuf) msg

let rec loop (next_token : unit -> Parser.token * Lexing.position * Lexing.position)
    (token_list : (string * Parser.token) list) (lexbuf : lexbuf)
    (last_input_needed : 'semantic_value I.env option) (checkpoint : 'semantic_value I.checkpoint) :
    Ast.source_file_or_master =
  match checkpoint with
  | I.InputNeeded env ->
      let token = next_token () in
      let checkpoint = I.offer checkpoint token in
      loop next_token token_list lexbuf (Some env) checkpoint
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      loop next_token token_list lexbuf last_input_needed checkpoint
  | I.HandlingError env -> fail lexbuf env token_list last_input_needed
  | I.Accepted v -> v
  | I.Rejected ->
      (* Cannot happen as we stop at syntax error immediatly *)
      assert false

let sedlex_with_menhir (lexer' : lexbuf -> Parser.token) (token_list : (string * Parser.token) list)
    (target_rule : Lexing.position -> 'semantic_value I.checkpoint) (lexbuf : lexbuf) :
    Ast.source_file_or_master =
  let lexer : unit -> Parser.token * Lexing.position * Lexing.position =
    with_tokenizer lexer' lexbuf
  in
  try loop lexer token_list lexbuf None (target_rule (fst @@ Sedlexing.lexing_positions lexbuf))
  with Sedlexing.MalFormed | Sedlexing.InvalidCodepoint _ ->
    Errors.lexer_error (lexing_positions lexbuf) (Utf8.lexeme lexbuf)

let rec parse_source_files (source_files : string list) (language : Cli.frontend_lang) : Ast.program
    =
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
          match language with
          | `Fr -> Lexer_fr.lexer_fr
          | `En -> Lexer_en.lexer_en
          | `NonVerbose -> Lexer.lexer
        in
        let token_list_lang =
          match language with
          | `Fr -> Lexer_fr.token_list_fr
          | `En -> Lexer_en.token_list_en
          | `NonVerbose -> Lexer.token_list
        in
        let commands_or_includes =
          sedlex_with_menhir lexer_lang token_list_lang Parser.Incremental.source_file_or_master
            lexbuf
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
