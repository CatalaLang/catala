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

open Catala_utils

(* type scope = { *)
(*   open_token_p : Tokens.token -> bool; *)
(*   end_token_p : Tokens.token -> bool; *)
(* } *)

(* let toplevel_scope = *)
(*   let open Tokens in *)
(*   let open_token_p = function *)
(*     | BEGIN_CODE | BEGIN_METADATA -> true *)
(*     | _ -> false *)
(*   in *)
(*   let end_token_p = function END_CODE _ -> true | t -> open_token_p t in *)
(*   { open_token_p; end_token_p } *)

(* let scopelevel_scope = *)
(*   let open Tokens in *)
(*   let open_token_p = function *)
(*     | DECLARATION | BEGIN_METADATA -> true *)
(*     | _ -> false *)
(*   in *)
(*   let end_token_p = function END_CODE _ -> true | t -> open_token_p t in *)
(*   { open_token_p; end_token_p } *)

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
    let context = ref `Law in
    let rec aux () =
      match lex_line ~context lexbuf with
      | Some (str, tok) ->
        Seq.Cons ((str, tok, Sedlexing.lexing_bytes_positions lexbuf), aux)
      | None ->
        close_in input;
        Seq.Nil
    in
    Seq.once aux
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

(** Parses a single source file *)
let rec parse_source ?resolve_included_file (lexbuf : Sedlexing.lexbuf) :
    Ast.program =
  let source_file_name = lexbuf_file lexbuf in
  Message.debug "Parsing %a" File.format source_file_name;
  let language = Cli.file_lang source_file_name in
  let commands =
    match Parser_state.with_state (Parser_loop.parse language) lexbuf with
    | Parser_loop.Ok r -> r
    | _ -> assert false
  in
  let program =
    expand_includes ?resolve_included_file source_file_name commands
  in
  {
    program with
    program_source_files = source_file_name :: program.Ast.program_source_files;
    program_lang = language;
  }

(** Expands the include directives in a parsing result, thus parsing new source
    files *)
and expand_includes
    ?(resolve_included_file = fun path -> Catala_utils.Global.FileName path)
    (source_file : string)
    (commands : Ast.law_structure list) : Ast.program =
  let language = Cli.file_lang source_file in
  let rprg =
    List.fold_left
      (fun acc command ->
        let join_module_names name_opt =
          match acc.Ast.program_module, name_opt with
          | opt, None | None, opt -> opt
          | Some id1, Some id2 ->
            Message.error ~kind:Parsing
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
          let pos = Mark.get inc_file in
          if File.check_file sub_source = None then
            Message.delayed_error ~kind:Parsing ~pos acc
              "Included file '%s' is not a regular file or does not exist."
              sub_source
          else
            let sub_source = resolve_included_file sub_source in
            with_sedlex_source sub_source
            @@ fun lexbuf ->
            let includ_program = parse_source ~resolve_included_file lexbuf in
            let () =
              includ_program.Ast.program_module
              |> Option.iter
                 @@ fun id ->
                 Message.error ~kind:Parsing
                   ~extra_pos:
                     [
                       "File include", Mark.get inc_file;
                       "Module declaration", Mark.get id.Ast.module_name;
                     ]
                   "A file that declares a module cannot be used through the \
                    raw '@{<yellow>> Include@}'@ directive.@ You should use it \
                    as a module with@ '@{<yellow>> Use @{<blue>%s@}@}'@ \
                    instead."
                   (Mark.remove id.Ast.module_name)
            in
            {
              Ast.program_module = acc.program_module;
              Ast.program_source_files =
                List.rev_append includ_program.program_source_files
                  acc.Ast.program_source_files;
              Ast.program_items =
                List.rev_append includ_program.program_items
                  acc.Ast.program_items;
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

(** {1 API} *)

let check_modname program source_file =
  match program.Ast.program_module, source_file with
  | ( Some { module_name = mname, pos; _ },
      (Global.FileName file | Global.Contents (_, file) | Global.Stdin file) )
    ->
    let basename_no_ext = File.remove_extension (Filename.basename file) in
    if File.equal (String.to_id mname) (String.to_id basename_no_ext) then ()
    else
      Message.error ~kind:Parsing ~pos
        "Module declared as@ @{<blue>%s@},@ which@ does@ not@ match@ the@ \
         file@ name@ %a.@ Rename the module to@ @{<blue>%s@}@ or@ the@ file@ \
         to@ %a."
        mname File.format file
        (String.capitalize_ascii basename_no_ext)
        File.format
        File.((dirname file / mname) -.- extension file)
  | _ -> ()

let load_source_file ?default_module_name ~is_stdlib source_file content_builder
    =
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
      Message.error ~kind:Parsing
        "%a doesn't define a module name. It should contain a '@{<cyan>> \
         Module %s@}' directive."
        File.format
        (Global.input_src_file source_file)
        (match source_file with
        | FileName s ->
          String.capitalize_ascii (Filename.basename (File.remove_extension s))
        | _ -> "Module_name")
  in
  let used_modules, module_items = content_builder program in
  {
    Ast.module_modname = modname;
    module_items;
    module_is_stdlib = is_stdlib;
    module_submodules = used_modules;
  }

let load_interface ?default_module_name ~is_stdlib source_file =
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
      | Ast.CodeBlock (code, _, is_metadata) ->
        (* Non-metadata blocks are ignored ; except for types that can
           automatically get exported if required by public or test items *)
        ( req,
          List.fold_left
            (fun acc -> function
              | Ast.ScopeUse _, _ -> acc
              | ( ( Ast.ScopeDecl _ | StructDecl _ | EnumDecl _
                  | AbstractTypeDecl _ ),
                  _ ) as e ->
                ( e,
                  if is_metadata then Shared_ast.Public else Shared_ast.Private
                )
                :: acc
              | Ast.Topdef def, m ->
                if is_metadata then
                  ((Ast.Topdef { def with topdef_expr = None }, m), Public)
                  :: acc
                else acc)
            acc code )
    in
    let req, acc = List.fold_left filter ([], []) program.Ast.program_items in
    List.rev req, Ast.Interface (List.rev acc)
  in
  load_source_file ?default_module_name ~is_stdlib source_file get_interface

let load_interface_and_code ?default_module_name ~is_stdlib source_file =
  let get_code_block program =
    let rec filter req = function
      | Ast.LawInclude _ | Ast.LawText _ | Ast.ModuleDef _ -> req
      | Ast.LawHeading (_, str) -> List.fold_left filter req str
      | Ast.ModuleUse (mod_use_name, alias) ->
        {
          Ast.mod_use_name;
          mod_use_alias = Option.value ~default:mod_use_name alias;
        }
        :: req
      | Ast.CodeBlock _ -> req
    in
    let mod_uses = List.fold_left filter [] program.Ast.program_items in
    List.rev mod_uses, Ast.Code program.Ast.program_items
  in
  load_source_file ?default_module_name ~is_stdlib source_file get_code_block

let resolution_tbl = Hashtbl.create 13

let register_included_file_resolver ~filename:s ~new_content =
  Hashtbl.replace resolution_tbl s new_content

let parse_top_level_file
    ?resolve_included_file
    (source_file : File.t Global.input_src) : Ast.program =
  let resolve_included_file =
    let tbl_lookup s = Hashtbl.find_opt resolution_tbl s in
    match resolve_included_file with
    | None -> fun s -> Option.value (tbl_lookup s) ~default:(Global.FileName s)
    | Some f -> f
  in
  let program =
    with_sedlex_source source_file (parse_source ~resolve_included_file)
  in
  check_modname program source_file;
  {
    program with
    Ast.program_items = law_struct_list_to_tree program.Ast.program_items;
  }
