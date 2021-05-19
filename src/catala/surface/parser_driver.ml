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

(** Wrapping module around parser and lexer that offers the {!val: parse_source_file} API *)

open Sedlexing
open Utils

(** {1 Internal functions} *)

(** Three-way minimum *)
let minimum a b c = min a (min b c)

(** Computes the levenshtein distance between two strings, used to provide error messages
    suggestions *)
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

(** After parsing, heading structure is completely flat because of the [source_file_item] rule. We
    need to tree-i-fy the flat structure, by looking at the precedence of the law headings. *)
let rec law_struct_list_to_tree (f : Ast.law_structure list) : Ast.law_structure list =
  match f with
  | [] -> []
  | [ item ] -> [ item ]
  | first_item :: rest -> (
      let rest_tree = law_struct_list_to_tree rest in
      match rest_tree with
      | [] -> assert false (* there should be at least one rest element *)
      | rest_head :: rest_tail -> (
          match first_item with
          | CodeBlock _ | LawText _ | LawInclude _ ->
              (* if an article or an include is just before a new heading , then we don't merge it
                 with what comes next *)
              first_item :: rest_head :: rest_tail
          | LawHeading (heading, _) ->
              (* here we have encountered a heading, which is going to "gobble" everything in the
                 [rest_tree] until it finds a heading of at least the same precedence *)
              let rec split_rest_tree (rest_tree : Ast.law_structure list) :
                  Ast.law_structure list * Ast.law_structure list =
                match rest_tree with
                | [] -> ([], [])
                | LawHeading (new_heading, _) :: _
                  when new_heading.law_heading_precedence <= heading.law_heading_precedence ->
                    (* we stop gobbling *)
                    ([], rest_tree)
                | first :: after ->
                    (* we continue gobbling *)
                    let after_gobbled, after_out = split_rest_tree after in
                    (first :: after_gobbled, after_out)
              in
              let gobbled, rest_out = split_rest_tree rest_tree in
              LawHeading (heading, gobbled) :: rest_out))

(** Style with which to display syntax hints in the terminal output *)
let syntax_hints_style = [ ANSITerminal.yellow ]

(** Usage: [raise_parser_error error_loc last_good_loc token msg]

    Raises an error message featuring the [error_loc] position where the parser has failed, the
    [token] on which the parser has failed, and the error message [msg]. If available, displays
    [last_good_loc] the location of the last token correctly parsed. *)
let raise_parser_error (error_loc : Pos.t) (last_good_loc : Pos.t option) (token : string)
    (msg : string) : 'a =
  Errors.raise_multispanned_error
    (Printf.sprintf "Syntax error at token %s\n%s"
       (Cli.print_with_style syntax_hints_style "\"%s\"" token)
       msg)
    ((Some "Error token:", error_loc)
     ::
     (match last_good_loc with
     | None -> []
     | Some last_good_loc -> [ (Some "Last good token:", last_good_loc) ]))

module ParserAux (LocalisedLexer : Lexer.LocalisedLexer) = struct
  include Parser.Make (LocalisedLexer)
  module I = MenhirInterpreter

  (** Returns the state number from the Menhir environment *)
  let state (env : 'semantic_value I.env) : int =
    match Lazy.force (I.stack env) with
    | MenhirLib.General.Nil -> 0
    | MenhirLib.General.Cons (Element (s, _, _, _), _) -> I.number s

  (** Usage: [fail lexbuf env token_list last_input_needed]

      Raises an error with meaningful hints about what the parsing error was. [lexbuf] is the lexing
      buffer state at the failure point, [env] is the Menhir environment and [last_input_needed] is
      the last checkpoint of a valid Menhir state before the parsing error. [token_list] is provided
      by things like {!val: Surface.Lexer.token_list_language_agnostic} and is used to provide
      suggestions of the tokens acceptable at the failure point *)
  let fail (lexbuf : lexbuf) (env : 'semantic_value I.env)
      (token_list : (string * Tokens.token) list) (last_input_needed : 'semantic_value I.env option)
      : 'a =
    let wrong_token = Utf8.lexeme lexbuf in
    let acceptable_tokens, last_positions =
      match last_input_needed with
      | Some last_input_needed ->
          ( List.filter
              (fun (_, t) ->
                I.acceptable (I.input_needed last_input_needed) t (fst (lexing_positions lexbuf)))
              token_list,
            Some (I.positions last_input_needed) )
      | None -> (token_list, None)
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
                (List.map
                   (fun (ts, _) -> Cli.print_with_style syntax_hints_style "\"%s\"" ts)
                   similar_acceptable_tokens)))
    in
    (* The parser has suspended itself because of a syntax error. Stop. *)
    let custom_menhir_message =
      match Parser_errors.message (state env) with
      | exception Not_found ->
          "Message: " ^ Cli.print_with_style syntax_hints_style "%s" "unexpected token"
      | msg ->
          "Message: "
          ^ Cli.print_with_style syntax_hints_style "%s"
              (String.trim (String.uncapitalize_ascii msg))
    in
    let msg =
      match similar_token_msg with
      | None -> custom_menhir_message
      | Some similar_token_msg ->
          Printf.sprintf "%s\nAutosuggestion: %s" custom_menhir_message similar_token_msg
    in
    raise_parser_error
      (Pos.from_lpos (lexing_positions lexbuf))
      (Option.map Pos.from_lpos last_positions)
      (Utf8.lexeme lexbuf) msg

  (** Main parsing loop *)
  let rec loop (next_token : unit -> Tokens.token * Lexing.position * Lexing.position)
      (token_list : (string * Tokens.token) list) (lexbuf : lexbuf)
      (last_input_needed : 'semantic_value I.env option) (checkpoint : 'semantic_value I.checkpoint)
      : Ast.source_file =
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

  (** Stub that wraps the parsing main loop and handles the Menhir/Sedlex type difference for
      [lexbuf]. *)
  let sedlex_with_menhir (lexer' : lexbuf -> Tokens.token)
      (token_list : (string * Tokens.token) list)
      (target_rule : Lexing.position -> 'semantic_value I.checkpoint) (lexbuf : lexbuf) :
      Ast.source_file =
    let lexer : unit -> Tokens.token * Lexing.position * Lexing.position =
      with_tokenizer lexer' lexbuf
    in
    try loop lexer token_list lexbuf None (target_rule (fst @@ Sedlexing.lexing_positions lexbuf))
    with Sedlexing.MalFormed | Sedlexing.InvalidCodepoint _ ->
      Lexer.raise_lexer_error (Pos.from_lpos (lexing_positions lexbuf)) (Utf8.lexeme lexbuf)

  let commands_or_includes (lexbuf : lexbuf) : Ast.source_file =
    sedlex_with_menhir LocalisedLexer.lexer LocalisedLexer.token_list Incremental.source_file lexbuf
end

module Parser_NonVerbose = ParserAux (Lexer)
module Parser_En = ParserAux (Lexer_en)
module Parser_Fr = ParserAux (Lexer_fr)
module Parser_Pl = ParserAux (Lexer_pl)

let localised_parser : Cli.frontend_lang -> lexbuf -> Ast.source_file = function
  | `NonVerbose -> Parser_NonVerbose.commands_or_includes
  | `En -> Parser_En.commands_or_includes
  | `Fr -> Parser_Fr.commands_or_includes
  | `Pl -> Parser_Pl.commands_or_includes

(** {1 Parsing multiple files} *)

(** Parses a single source file *)
let rec parse_source_file (source_file : Pos.input_file) (language : Cli.frontend_lang) :
    Ast.program =
  Cli.debug_print
    (Printf.sprintf "Parsing %s" (match source_file with FileName s | Contents s -> s));
  let lexbuf, input =
    match source_file with
    | FileName source_file -> (
        try
          let input = open_in source_file in
          (Sedlexing.Utf8.from_channel input, Some input)
        with Sys_error msg -> Errors.raise_error msg)
    | Contents contents -> (Sedlexing.Utf8.from_gen (Gen.of_string contents), None)
  in
  let source_file_name = match source_file with FileName s -> s | Contents _ -> "stdin" in
  Sedlexing.set_filename lexbuf source_file_name;
  Parse_utils.current_file := source_file_name;
  let commands = localised_parser language lexbuf in
  (match input with Some input -> close_in input | None -> ());
  let program = expand_includes source_file_name commands language in
  {
    program_items = program.Ast.program_items;
    program_source_files = source_file_name :: program.Ast.program_source_files;
  }

(** Expands the include directives in a parsing result, thus parsing new source files *)
and expand_includes (source_file : string) (commands : Ast.law_structure list)
    (language : Cli.frontend_lang) : Ast.program =
  List.fold_left
    (fun acc command ->
      match command with
      | Ast.LawInclude (Ast.CatalaFile sub_source) ->
          let source_dir = Filename.dirname source_file in
          let sub_source = Filename.concat source_dir (Pos.unmark sub_source) in
          let includ_program = parse_source_file (FileName sub_source) language in
          {
            Ast.program_source_files =
              acc.Ast.program_source_files @ includ_program.program_source_files;
            Ast.program_items = acc.Ast.program_items @ includ_program.program_items;
          }
      | Ast.LawHeading (heading, commands') ->
          let { Ast.program_items = commands'; Ast.program_source_files = new_sources } =
            expand_includes source_file commands' language
          in
          {
            Ast.program_source_files = acc.Ast.program_source_files @ new_sources;
            Ast.program_items = acc.Ast.program_items @ [ Ast.LawHeading (heading, commands') ];
          }
      | i -> { acc with Ast.program_items = acc.Ast.program_items @ [ i ] })
    { Ast.program_source_files = []; Ast.program_items = [] }
    commands

(** {1 API} *)

let parse_top_level_file (source_file : Pos.input_file) (language : Cli.frontend_lang) : Ast.program
    =
  let program = parse_source_file source_file language in
  { program with Ast.program_items = law_struct_list_to_tree program.Ast.program_items }
