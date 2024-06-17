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

module ParserAux (LocalisedLexer : Lexer_common.LocalisedLexer) = struct
  include Parser.Make (LocalisedLexer)
  module I = MenhirInterpreter

  (** Returns the state number from the Menhir environment *)
  let state (env : 'semantic_value I.env) : int =
    match Lazy.force (I.stack env) with
    | MenhirLib.General.Nil -> 0
    | MenhirLib.General.Cons (Element (s, _, _, _), _) -> I.number s

  let register_parsing_error
      (lexbuf : lexbuf)
      (env : 'semantic_value I.env)
      (acceptable_tokens : (string * Tokens.token) list)
      (similar_candidate_tokens : string list) : 'a =
    (* The parser has suspended itself because of a syntax error. *)
    let custom_menhir_message ppf =
      (match Parser_errors.message (state env) with
      | exception Not_found -> Format.fprintf ppf "@{<yellow>unexpected token@}"
      | msg ->
        Format.fprintf ppf "@{<yellow>@<1>%s@} @[<hov>%a@]" "»"
          Format.pp_print_text
          (String.trim (String.uncapitalize_ascii msg)));
      if acceptable_tokens <> [] then
        Format.fprintf ppf "@\n@[<hov>Those are valid at this point:@ %a@]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
             (fun ppf string -> Format.fprintf ppf "@{<yellow>\"%s\"@}" string))
          (List.map (fun (s, _) -> s) acceptable_tokens)
    in
    let suggestion =
      if similar_candidate_tokens = [] then None
      else Some similar_candidate_tokens
    in
    let error_loc = Pos.from_lpos (lexing_positions lexbuf) in
    let wrong_token = Utf8.lexeme lexbuf in
    let msg = custom_menhir_message in
    Message.delayed_error () ?suggestion
      ~extra_pos:["", error_loc]
      "@[<hov>Syntax error at %a:@ %t@]"
      (fun ppf string -> Format.fprintf ppf "@{<yellow>\"%s\"@}" string)
      wrong_token msg

  let sorted_candidate_tokens lexbuf token_list env =
    let acceptable_tokens =
      List.filter
        (fun (_, t) ->
          I.acceptable (I.input_needed env) t (fst (lexing_positions lexbuf)))
        token_list
    in
    let similar_acceptable_tokens =
      Suggestions.suggestion_minimum_levenshtein_distance_association
        (List.map (fun (s, _) -> s) acceptable_tokens)
        (Utf8.lexeme lexbuf)
    in
    let module S = Set.Make (String) in
    let s_toks = S.of_list similar_acceptable_tokens in
    let sorted_acceptable_tokens =
      List.sort
        (fun (s, _) _ -> if S.mem s s_toks then -1 else 1)
        acceptable_tokens
    in
    similar_acceptable_tokens, sorted_acceptable_tokens

  type 'a ring_buffer = {
    curr_idx : int;
    start : int ref;
    stop : int ref;
    max_size : int;
    feed : unit -> 'a;
    data : 'a array;
  }

  let next ({ curr_idx; start; stop; max_size; feed; data } as buff) =
    let next_idx = succ curr_idx mod max_size in
    if curr_idx = !stop then (
      let new_elt = feed () in
      data.(curr_idx) <- new_elt;
      let size = ((!stop - !start + max_size) mod max_size) + 1 in
      stop := succ !stop mod max_size;
      let is_full = size = max_size in
      if is_full then
        (* buffer will get full: start is also moved *)
        start := succ !start mod max_size;
      { buff with curr_idx = next_idx }, new_elt)
    else
      let elt = data.(curr_idx) in
      { buff with curr_idx = next_idx }, elt

  let create ?(max_size = 20) feed v =
    {
      curr_idx = 0;
      start = ref 0;
      stop = ref 0;
      feed;
      data = Array.make max_size v;
      max_size;
    }

  let progress ?(max_step = 10) lexer_buffer env checkpoint : int =
    let rec loop nth_step lexer_buffer env checkpoint =
      if nth_step >= max_step then nth_step
      else
        match checkpoint with
        | I.InputNeeded env ->
          let new_lexer_buffer, token = next lexer_buffer in
          let checkpoint = I.offer checkpoint token in
          loop (succ nth_step) new_lexer_buffer env checkpoint
        | I.Shifting _ | I.AboutToReduce _ ->
          let checkpoint = I.resume checkpoint in
          loop nth_step lexer_buffer env checkpoint
        | I.HandlingError (_ : _ I.env) | I.Accepted _ | I.Rejected -> nth_step
    in
    loop 0 lexer_buffer env checkpoint

  let recover_parsing_error lexer_buffer env acceptable_tokens =
    let candidates_checkpoints =
      let without_token = I.input_needed env in
      let make_with_token tok =
        let l, r = I.positions env in
        let checkpoint = I.input_needed env in
        I.offer checkpoint (tok, l, r)
      in
      without_token :: List.map make_with_token acceptable_tokens
    in
    let threshold = min 10 lexer_buffer.max_size in
    let rec iterate ((curr_max_progress, _) as acc) = function
      | [] -> acc
      | cp :: t ->
        if curr_max_progress >= 10 then acc
        else
          let cp_progress = progress ~max_step:threshold lexer_buffer env cp in
          if cp_progress > curr_max_progress then iterate (cp_progress, cp) t
          else iterate acc t
    in
    let best_progress, best_cp =
      let dummy_cp = I.input_needed env in
      iterate (-1, dummy_cp) candidates_checkpoints
    in
    (* We do not consider paths were progress isn't significant *)
    if best_progress < 2 then None else Some best_cp

  (** Main parsing loop *)
  let loop
      (lexer_buffer :
        (Tokens.token * Lexing.position * Lexing.position) ring_buffer)
      (token_list : (string * Tokens.token) list)
      (lexbuf : lexbuf)
      (last_input_needed : 'semantic_value I.env option)
      (checkpoint : 'semantic_value I.checkpoint) : Ast.source_file =
    let rec loop
        (lexer_buffer :
          (Tokens.token * Lexing.position * Lexing.position) ring_buffer)
        (token_list : (string * Tokens.token) list)
        (lexbuf : lexbuf)
        (last_input_needed : 'semantic_value I.env option)
        (checkpoint : 'semantic_value I.checkpoint) : Ast.source_file =
      match checkpoint with
      | I.InputNeeded env ->
        let new_lexer_buffer, token = next lexer_buffer in
        let checkpoint = I.offer checkpoint token in
        loop new_lexer_buffer token_list lexbuf (Some env) checkpoint
      | I.Shifting _ | I.AboutToReduce _ ->
        let checkpoint = I.resume checkpoint in
        loop lexer_buffer token_list lexbuf last_input_needed checkpoint
      | I.HandlingError (env : 'semantic_value I.env) -> (
        let similar_candidate_tokens, sorted_acceptable_tokens =
          sorted_candidate_tokens lexbuf token_list env
        in
        register_parsing_error lexbuf env sorted_acceptable_tokens
          similar_candidate_tokens;
        let best_effort_checkpoint =
          recover_parsing_error lexer_buffer env
            (List.map snd sorted_acceptable_tokens)
        in
        match best_effort_checkpoint with
        | None ->
          (* No reasonable solution, aborting *)
          []
        | Some best_effort_checkpoint ->
          loop lexer_buffer token_list lexbuf last_input_needed
            best_effort_checkpoint)
      | I.Accepted v -> v
      | I.Rejected -> []
    in
    loop lexer_buffer token_list lexbuf last_input_needed checkpoint

  (** Stub that wraps the parsing main loop and handles the Menhir/Sedlex type
      difference for [lexbuf]. *)
  let sedlex_with_menhir
      (lexer' : lexbuf -> Tokens.token)
      (token_list : (string * Tokens.token) list)
      (target_rule : Lexing.position -> 'semantic_value I.checkpoint)
      (lexbuf : lexbuf) : Ast.source_file =
    let lexer_buffer :
        (Tokens.token * Lexing.position * Lexing.position) ring_buffer =
      let feed = with_tokenizer lexer' lexbuf in
      create feed Lexing.(Tokens.EOF, dummy_pos, dummy_pos)
    in
    try
      let target_rule =
        target_rule (fst @@ Sedlexing.lexing_positions lexbuf)
      in
      Message.with_delayed_errors
      @@ fun () -> loop lexer_buffer token_list lexbuf None target_rule
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

let localised_parser : Global.backend_lang -> lexbuf -> Ast.source_file =
  function
  | En -> Parser_En.commands_or_includes
  | Fr -> Parser_Fr.commands_or_includes
  | Pl -> Parser_Pl.commands_or_includes

(** Lightweight lexer for dependency *)

let lines (file : File.t) (language : Global.backend_lang) =
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
  Message.debug "Parsing %a" File.format source_file_name;
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
          match acc.Ast.program_module, name_opt with
          | opt, None | None, opt -> opt
          | Some id1, Some id2 ->
            Message.error
              ~extra_pos:
                ["", Mark.get id1.module_name; "", Mark.get id2.module_name]
              "Multiple definitions of the module name"
        in
        match command with
        | Ast.ModuleDef (id, is_external) ->
          {
            acc with
            Ast.program_module =
              join_module_names
                (Some { module_name = id; module_external = is_external });
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
            includ_program.Ast.program_module
            |> Option.iter
               @@ fun id ->
               Message.error
                 ~extra_pos:
                   [
                     "File include", Mark.get inc_file;
                     "Module declaration", Mark.get id.Ast.module_name;
                   ]
                 "A file that declares a module cannot be used through the raw \
                  '@{<yellow>> Include@}'@ directive.@ You should use it as a \
                  module with@ '@{<yellow>> Use @{<blue>%s@}@}'@ instead."
                 (Mark.remove id.Ast.module_name)
          in
          {
            Ast.program_module = acc.program_module;
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
            Ast.program_module;
            Ast.program_items = commands';
            Ast.program_source_files = new_sources;
            Ast.program_used_modules = new_used_modules;
            Ast.program_lang = _;
          } =
            expand_includes source_file commands'
          in
          {
            Ast.program_module = join_module_names program_module;
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
        Ast.program_module = None;
        Ast.program_source_files = [];
        Ast.program_items = [];
        Ast.program_used_modules = [];
        Ast.program_lang = language;
      }
      commands
  in
  {
    Ast.program_lang = language;
    Ast.program_module = rprg.Ast.program_module;
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
  | Global.FileName file -> with_sedlex_file file f
  | Global.Contents (str, file) ->
    let lexbuf = Sedlexing.Utf8.from_string str in
    Sedlexing.set_filename lexbuf file;
    f lexbuf
  | Global.Stdin file ->
    let lexbuf = Sedlexing.Utf8.from_channel stdin in
    Sedlexing.set_filename lexbuf file;
    f lexbuf

let check_modname program source_file =
  match program.Ast.program_module, source_file with
  | ( Some { module_name = mname, pos; _ },
      (Global.FileName file | Global.Contents (_, file) | Global.Stdin file) )
    when not File.(equal mname Filename.(remove_extension (basename file))) ->
    Message.error ~pos
      "Module declared as@ @{<blue>%s@},@ which@ does@ not@ match@ the@ file@ \
       name@ %a.@ Rename the module to@ @{<blue>%s@}@ or@ the@ file@ to@ %a."
      mname File.format file
      (String.capitalize_ascii Filename.(remove_extension (basename file)))
      File.format
      File.((dirname file / mname) ^ Filename.extension file)
  | _ -> ()

let load_interface ?default_module_name source_file =
  let program = with_sedlex_source source_file parse_source in
  check_modname program source_file;
  let modname =
    match program.Ast.program_module, default_module_name with
    | Some mname, _ -> mname
    | None, Some n ->
      {
        module_name =
          n, Pos.from_info (Global.input_src_file source_file) 0 0 0 0;
        module_external = false;
      }
    | None, None ->
      Message.error
        "%a doesn't define a module name. It should contain a '@{<cyan>> \
         Module %s@}' directive."
        File.format
        (Global.input_src_file source_file)
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

let parse_top_level_file (source_file : File.t Global.input_src) : Ast.program =
  let program = with_sedlex_source source_file parse_source in
  check_modname program source_file;
  {
    program with
    Ast.program_items = law_struct_list_to_tree program.Ast.program_items;
  }
