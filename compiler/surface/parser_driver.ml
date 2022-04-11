(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Wrapping module around parser and lexer that offers the {!:
    Parser_driver.parse_source_file} API. *)

open Sedlexing
open Utils

(** {1 Internal functions} *)

(** Three-way minimum *)

(** After parsing, heading structure is completely flat because of the
    [source_file_item] rule. We need to tree-i-fy the flat structure, by looking
    at the precedence of the law headings. *)
let rec law_struct_list_to_tree (f : Ast.law_structure list) :
    Ast.law_structure list =
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
              (* if an article or an include is just before a new heading , then
                 we don't merge it with what comes next *)
              first_item :: rest_head :: rest_tail
          | LawHeading (heading, _) ->
              (* here we have encountered a heading, which is going to "gobble"
                 everything in the [rest_tree] until it finds a heading of at
                 least the same precedence *)
              let rec split_rest_tree (rest_tree : Ast.law_structure list) :
                  Ast.law_structure list * Ast.law_structure list =
                match rest_tree with
                | [] -> ([], [])
                | LawHeading (new_heading, _) :: _
                  when new_heading.law_heading_precedence
                       <= heading.law_heading_precedence ->
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

module ParserAux (LocalisedLexer : Lexer_common.LocalisedLexer) = struct
  include Parser.Make (LocalisedLexer)

  (** Main parsing loop *)

  (** Stub that wraps the parsing main loop and handles the Menhir/Sedlex type
      difference for [lexbuf]. *)

  let commands_or_includes (lexbuf : lexbuf) : Ast.source_file =
    let revised_lexer = with_tokenizer LocalisedLexer.lexer lexbuf in
    let revised_parser =
      MenhirLib.Convert.Simplified.traditional2revised source_file
    in
    revised_parser revised_lexer
end

module Parser_En = ParserAux (Lexer_en)
module Parser_Fr = ParserAux (Lexer_fr)
module Parser_Pl = ParserAux (Lexer_pl)

let localised_parser : Cli.backend_lang -> lexbuf -> Ast.source_file = function
  | En -> Parser_En.commands_or_includes
  | Fr -> Parser_Fr.commands_or_includes
  | Pl -> Parser_Pl.commands_or_includes

(** {1 Parsing multiple files} *)

(** Parses a single source file *)
let rec parse_source_file
    (source_file : Pos.input_file) (language : Cli.backend_lang) : Ast.program =
  Cli.debug_print "Parsing %s"
    (match source_file with FileName s | Contents s -> s);
  let lexbuf, input =
    match source_file with
    | FileName source_file -> (
        try
          let input = open_in source_file in
          (Sedlexing.Utf8.from_channel input, Some input)
        with Sys_error msg -> Errors.raise_error "%s" msg)
    | Contents contents -> (Sedlexing.Utf8.from_string contents, None)
  in
  let source_file_name =
    match source_file with FileName s -> s | Contents _ -> "stdin"
  in
  Sedlexing.set_filename lexbuf source_file_name;
  Parse_utils.current_file := source_file_name;
  let commands = localised_parser language lexbuf in
  (match input with Some input -> close_in input | None -> ());
  let program = expand_includes source_file_name commands language in
  {
    program_items = program.Ast.program_items;
    program_source_files = source_file_name :: program.Ast.program_source_files;
  }

(** Expands the include directives in a parsing result, thus parsing new source
    files *)
and expand_includes
    (source_file : string)
    (commands : Ast.law_structure list)
    (language : Cli.backend_lang) : Ast.program =
  List.fold_left
    (fun acc command ->
      match command with
      | Ast.LawInclude (Ast.CatalaFile sub_source) ->
          let source_dir = Filename.dirname source_file in
          let sub_source = Filename.concat source_dir (Pos.unmark sub_source) in
          let includ_program =
            parse_source_file (FileName sub_source) language
          in
          {
            Ast.program_source_files =
              acc.Ast.program_source_files @ includ_program.program_source_files;
            Ast.program_items =
              acc.Ast.program_items @ includ_program.program_items;
          }
      | Ast.LawHeading (heading, commands') ->
          let {
            Ast.program_items = commands';
            Ast.program_source_files = new_sources;
          } =
            expand_includes source_file commands' language
          in
          {
            Ast.program_source_files =
              acc.Ast.program_source_files @ new_sources;
            Ast.program_items =
              acc.Ast.program_items @ [ Ast.LawHeading (heading, commands') ];
          }
      | i -> { acc with Ast.program_items = acc.Ast.program_items @ [ i ] })
    { Ast.program_source_files = []; Ast.program_items = [] }
    commands

(** {1 API} *)

let parse_top_level_file
    (source_file : Pos.input_file) (language : Cli.backend_lang) : Ast.program =
  let program = parse_source_file source_file language in
  {
    program with
    Ast.program_items = law_struct_list_to_tree program.Ast.program_items;
  }
