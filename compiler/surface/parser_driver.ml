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

(** Wrapping module around parser and lexer that offers the
    {!:Parser_driver.parse_source_file} API. *)

open Sedlexing
open Catala_utils

(** After parsing, heading structure is completely flat because of the
    [source_file_item] rule. We need to tree-i-fy the flat structure, by looking
    at the precedence of the law headings. *)
let rec law_struct_list_to_tree (f : Ast.law_structure list) :
    Ast.law_structure list =
  match f with
  | [] -> []
  | [item] -> [item]
  | first_item :: rest -> (
    let rest_tree = law_struct_list_to_tree rest in
    match rest_tree with
    | [] -> assert false (* there should be at least one rest element *)
    | rest_head :: rest_tail -> (
      match first_item with
      | CodeBlock _ | LawText _ | LawInclude _ | ModuleDef _ | ModuleUse _ ->
        (* if an article or an include is just before a new heading , then we
           don't merge it with what comes next *)
        first_item :: rest_head :: rest_tail
      | LawHeading (heading, _) ->
        (* here we have encountered a heading, which is going to "gobble"
           everything in the [rest_tree] until it finds a heading of at least
           the same precedence *)
        let rec split_rest_tree (rest_tree : Ast.law_structure list) :
            Ast.law_structure list * Ast.law_structure list =
          match rest_tree with
          | [] -> [], []
          | LawHeading (new_heading, _) :: _
            when new_heading.law_heading_precedence
                 <= heading.law_heading_precedence ->
            (* we stop gobbling *)
            [], rest_tree
          | first :: after ->
            (* we continue gobbling *)
            let after_gobbled, after_out = split_rest_tree after in
            first :: after_gobbled, after_out
        in
        let gobbled, rest_out = split_rest_tree rest_tree in
        LawHeading (heading, gobbled) :: rest_out))

(** Usage: [raise_parser_error error_loc last_good_loc token msg]

    Raises an error message featuring the [error_loc] position where the parser
    has failed, the [token] on which the parser has failed, and the error
    message [msg]. If available, displays [last_good_loc] the location of the
    last token correctly parsed. *)
let raise_parser_error
    ?(suggestion : string list option)
    (error_loc : Pos.t)
    (last_good_loc : Pos.t option)
    (token : string)
    (msg : Format.formatter -> unit) : 'a =
  Message.raise_multispanned_error_full ?suggestion
    ((Some (fun ppf -> Format.pp_print_string ppf "Error token:"), error_loc)
    ::
    (match last_good_loc with
    | None -> []
    | Some last_good_loc ->
      [
        ( Some (fun ppf -> Format.pp_print_string ppf "Last good token:"),
          last_good_loc );
      ]))
    "@[<v>Syntax error at token %a@,%t@]"
    (fun ppf string -> Format.fprintf ppf "@{<yellow>\"%s\"@}" string)
    token msg

module ParserAux (LocalisedLexer : Lexer_common.LocalisedLexer) = struct
  include Parser.Make (LocalisedLexer)
  module I = MenhirInterpreter

  (** Returns the state number from the Menhir environment *)
  let state (env : 'semantic_value I.env) : int =
    match Lazy.force (I.stack env) with
    | MenhirLib.General.Nil -> 0
    | MenhirLib.General.Cons (Element (s, _, _, _), _) -> I.number s

  (** Usage: [fail lexbuf env token_list last_input_needed]

      Raises an error with meaningful hints about what the parsing error was.
      [lexbuf] is the lexing buffer state at the failure point, [env] is the
      Menhir environment and [last_input_needed] is the last checkpoint of a
      valid Menhir state before the parsing error. [token_list] is provided by
      things like {!val: Surface.Lexer_common.token_list_language_agnostic} and
      is used to provide suggestions of the tokens acceptable at the failure
      point *)
  let fail
      (lexbuf : lexbuf)
      (env : 'semantic_value I.env)
      (token_list : (string * Tokens.token) list)
      (last_input_needed : 'semantic_value I.env option) : 'a =
    let wrong_token = Utf8.lexeme lexbuf in
    let acceptable_tokens, last_positions =
      match last_input_needed with
      | Some last_input_needed ->
        ( List.filter
            (fun (_, t) ->
              I.acceptable
                (I.input_needed last_input_needed)
                t
                (fst (lexing_positions lexbuf)))
            token_list,
          Some (I.positions last_input_needed) )
      | None -> token_list, None
    in
    let similar_acceptable_tokens =
      Suggestions.suggestion_minimum_levenshtein_distance_association
        (List.map (fun (s, _) -> s) acceptable_tokens)
        wrong_token
    in
    (* The parser has suspended itself because of a syntax error. Stop. *)
    let custom_menhir_message ppf =
      (match Parser_errors.message (state env) with
      | exception Not_found ->
        Format.fprintf ppf "Message: @{<yellow>unexpected token@}@,%t"
      | msg ->
        Format.fprintf ppf "Message: @{<yellow>%s@}@,%t"
          (String.trim (String.uncapitalize_ascii msg)))
        (fun (ppf : Format.formatter) ->
          Format.fprintf ppf "You could have written : ";
          Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ or ")
            (fun ppf string -> Format.fprintf ppf "@{<yellow>\"%s\"@}" string)
            ppf
            (List.map (fun (s, _) -> s) acceptable_tokens))
    in
    raise_parser_error ~suggestion:similar_acceptable_tokens
      (Pos.from_lpos (lexing_positions lexbuf))
      (Option.map Pos.from_lpos last_positions)
      (Utf8.lexeme lexbuf) custom_menhir_message

  (** Main parsing loop *)
  let rec loop
      (next_token : unit -> Tokens.token * Lexing.position * Lexing.position)
      (token_list : (string * Tokens.token) list)
      (lexbuf : lexbuf)
      (last_input_needed : 'semantic_value I.env option)
      (checkpoint : 'semantic_value I.checkpoint) : Ast.source_file =
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

  (** Stub that wraps the parsing main loop and handles the Menhir/Sedlex type
      difference for [lexbuf]. *)
  let sedlex_with_menhir
      (lexer' : lexbuf -> Tokens.token)
      (token_list : (string * Tokens.token) list)
      (target_rule : Lexing.position -> 'semantic_value I.checkpoint)
      (lexbuf : lexbuf) : Ast.source_file =
    let lexer : unit -> Tokens.token * Lexing.position * Lexing.position =
      with_tokenizer lexer' lexbuf
    in
    try
      loop lexer token_list lexbuf None
        (target_rule (fst @@ Sedlexing.lexing_positions lexbuf))
    with Sedlexing.MalFormed | Sedlexing.InvalidCodepoint _ ->
      Lexer_common.raise_lexer_error
        (Pos.from_lpos (lexing_positions lexbuf))
        (Utf8.lexeme lexbuf)

  let commands_or_includes (lexbuf : lexbuf) : Ast.source_file =
    sedlex_with_menhir LocalisedLexer.lexer LocalisedLexer.token_list
      Incremental.source_file lexbuf
end

module Parser_En = ParserAux (Lexer_en)
module Parser_Fr = ParserAux (Lexer_fr)
module Parser_Pl = ParserAux (Lexer_pl)

let localised_parser : Cli.backend_lang -> lexbuf -> Ast.source_file = function
  | En -> Parser_En.commands_or_includes
  | Fr -> Parser_Fr.commands_or_includes
  | Pl -> Parser_Pl.commands_or_includes

(** Lightweight lexer for dependency *)

let lines (file : File.t) (language : Cli.backend_lang) =
  let lex_line =
    match language with
    | En -> Lexer_en.lex_line
    | Fr -> Lexer_fr.lex_line
    | Pl -> Lexer_pl.lex_line
  in
  let input = open_in file in
  try
    let lexbuf = Sedlexing.Utf8.from_channel input in
    Sedlexing.set_filename lexbuf file;
    let rec aux () =
      match lex_line lexbuf with
      | Some line -> Seq.Cons (line, aux)
      | None ->
        close_in input;
        Seq.Nil
    in
    aux
  with exc ->
    let bt = Printexc.get_raw_backtrace () in
    close_in input;
    Printexc.raise_with_backtrace exc bt

(** {1 Parsing multiple files} *)

let lexbuf_file lexbuf =
  (fst (Sedlexing.lexing_positions lexbuf)).Lexing.pos_fname

let with_sedlex_file file f =
  let ic = open_in file in
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  Sedlexing.set_filename lexbuf file;
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> f lexbuf)

(** Parses a single source file *)
let rec parse_source (lexbuf : Sedlexing.lexbuf) : Ast.program =
  let source_file_name = lexbuf_file lexbuf in
  Message.emit_debug "Parsing %a" File.format source_file_name;
  let language = Cli.file_lang source_file_name in
  let commands = localised_parser language lexbuf in
  let program = expand_includes source_file_name commands in
  {
    program with
    program_source_files = source_file_name :: program.Ast.program_source_files;
    program_lang = language;
  }

(** Expands the include directives in a parsing result, thus parsing new source
    files *)
and expand_includes (source_file : string) (commands : Ast.law_structure list) :
    Ast.program =
  let language = Cli.file_lang source_file in
  let rprg =
    List.fold_left
      (fun acc command ->
        let join_module_names name_opt =
          match acc.Ast.program_module_name, name_opt with
          | opt, None | None, opt -> opt
          | Some id1, Some id2 ->
            Message.raise_multispanned_error
              [None, Mark.get id1; None, Mark.get id2]
              "Multiple definitions of the module name"
        in
        match command with
        | Ast.ModuleDef (id, _) ->
          {
            acc with
            Ast.program_module_name = join_module_names (Some id);
            Ast.program_items = command :: acc.Ast.program_items;
          }
        | Ast.ModuleUse (mod_use_name, alias) ->
          let mod_use_alias = Option.value ~default:mod_use_name alias in
          {
            acc with
            Ast.program_used_modules =
              { mod_use_name; mod_use_alias } :: acc.Ast.program_used_modules;
            Ast.program_items = command :: acc.Ast.program_items;
          }
        | Ast.LawInclude (Ast.CatalaFile inc_file) ->
          let source_dir = Filename.dirname source_file in
          let sub_source = File.(source_dir / Mark.remove inc_file) in
          with_sedlex_file sub_source
          @@ fun lexbuf ->
          let includ_program = parse_source lexbuf in
          let () =
            includ_program.Ast.program_module_name
            |> Option.iter
               @@ fun id ->
               Message.raise_multispanned_error
                 [
                   Some "File include", Mark.get inc_file;
                   Some "Module declaration", Mark.get id;
                 ]
                 "A file that declares a module cannot be used through the raw \
                  '@{<yellow>> Include@}' directive. You should use it as a \
                  module with '@{<yellow>> Use @{<blue>%s@}@}' instead."
                 (Mark.remove id)
          in
          {
            Ast.program_module_name = acc.program_module_name;
            Ast.program_source_files =
              List.rev_append includ_program.program_source_files
                acc.Ast.program_source_files;
            Ast.program_items =
              List.rev_append includ_program.program_items acc.Ast.program_items;
            Ast.program_used_modules =
              List.rev_append includ_program.program_used_modules
                acc.Ast.program_used_modules;
            Ast.program_lang = language;
          }
        | Ast.LawHeading (heading, commands') ->
          let {
            Ast.program_module_name;
            Ast.program_items = commands';
            Ast.program_source_files = new_sources;
            Ast.program_used_modules = new_used_modules;
            Ast.program_lang = _;
          } =
            expand_includes source_file commands'
          in
          {
            Ast.program_module_name = join_module_names program_module_name;
            Ast.program_source_files =
              List.rev_append new_sources acc.Ast.program_source_files;
            Ast.program_items =
              Ast.LawHeading (heading, commands') :: acc.Ast.program_items;
            Ast.program_used_modules =
              List.rev_append new_used_modules acc.Ast.program_used_modules;
            Ast.program_lang = language;
          }
        | i -> { acc with Ast.program_items = i :: acc.Ast.program_items })
      {
        Ast.program_module_name = None;
        Ast.program_source_files = [];
        Ast.program_items = [];
        Ast.program_used_modules = [];
        Ast.program_lang = language;
      }
      commands
  in
  {
    Ast.program_lang = language;
    Ast.program_module_name = rprg.Ast.program_module_name;
    Ast.program_source_files = List.rev rprg.Ast.program_source_files;
    Ast.program_items = List.rev rprg.Ast.program_items;
    Ast.program_used_modules = List.rev rprg.Ast.program_used_modules;
  }

(** {2 Handling interfaces} *)

let get_interface program =
  let rec filter (req, acc) = function
    | Ast.LawInclude _ | Ast.LawText _ | Ast.ModuleDef _ -> req, acc
    | Ast.LawHeading (_, str) -> List.fold_left filter (req, acc) str
    | Ast.ModuleUse (mod_use_name, alias) ->
      ( {
          Ast.mod_use_name;
          mod_use_alias = Option.value ~default:mod_use_name alias;
        }
        :: req,
        acc )
    | Ast.CodeBlock (code, _, true) ->
      ( req,
        List.fold_left
          (fun acc -> function
            | Ast.ScopeUse _, _ -> acc
            | ((Ast.ScopeDecl _ | StructDecl _ | EnumDecl _), _) as e ->
              e :: acc
            | Ast.Topdef def, m ->
              (Ast.Topdef { def with topdef_expr = None }, m) :: acc)
          acc code )
    | Ast.CodeBlock (_, _, false) ->
      (* Non-metadata blocks are ignored *)
      req, acc
  in
  List.fold_left filter ([], []) program.Ast.program_items

(** {1 API} *)

let with_sedlex_source source_file f =
  match source_file with
  | Cli.FileName file -> with_sedlex_file file f
  | Cli.Contents (str, file) ->
    let lexbuf = Sedlexing.Utf8.from_string str in
    Sedlexing.set_filename lexbuf file;
    f lexbuf
  | Cli.Stdin file ->
    let lexbuf = Sedlexing.Utf8.from_channel stdin in
    Sedlexing.set_filename lexbuf file;
    f lexbuf

let check_modname program source_file =
  match program.Ast.program_module_name, source_file with
  | ( Some (mname, pos),
      (Cli.FileName file | Cli.Contents (_, file) | Cli.Stdin file) )
    when not File.(equal mname Filename.(remove_extension (basename file))) ->
    Message.raise_spanned_error pos
      "@[<hov>Module declared as@ @{<blue>%s@},@ which@ does@ not@ match@ the@ \
       file@ name@ %a.@ Rename the module to@ @{<blue>%s@}@ or@ the@ file@ to@ \
       %a.@]"
      mname File.format file
      (String.capitalize_ascii Filename.(remove_extension (basename file)))
      File.format
      File.((dirname file / mname) ^ Filename.extension file)
  | _ -> ()

let load_interface source_file =
  let program = with_sedlex_source source_file parse_source in
  check_modname program source_file;
  let modname =
    match program.Ast.program_module_name with
    | Some mname -> mname
    | None ->
      Message.raise_error
        "%a doesn't define a module name. It should contain a '@{<cyan>> \
         Module %s@}' directive."
        File.format
        (Cli.input_src_file source_file)
        (match source_file with
        | FileName s ->
          String.capitalize_ascii Filename.(basename (remove_extension s))
        | _ -> "Module_name")
  in
  let used_modules, intf = get_interface program in
  {
    Ast.intf_modname = modname;
    Ast.intf_code = intf;
    Ast.intf_submodules = used_modules;
  }

let parse_top_level_file (source_file : Cli.input_src) : Ast.program =
  let program = with_sedlex_source source_file parse_source in
  check_modname program source_file;
  {
    program with
    Ast.program_items = law_struct_list_to_tree program.Ast.program_items;
  }
