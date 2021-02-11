module I = Parser.MenhirInterpreter
val state : 'semantic_value I.env -> int
val minimum : 'a -> 'a -> 'a -> 'a
val levenshtein_distance : string -> string -> int
val law_struct_list_to_tree :
  Ast.program_item list -> Ast.program_item list
val syntax_hints_style : ANSITerminal.style list
val raise_parser_error :
  Utils.Pos.t -> Utils.Pos.t option -> string -> string -> 'a
val fail :
  Sedlexing.lexbuf ->
  'semantic_value I.env ->
  (string * Parser.token) list -> 'semantic_value I.env option -> 'a
val loop :
  (unit -> Parser.token * Lexing.position * Lexing.position) ->
  (string * Parser.token) list ->
  Sedlexing.lexbuf ->
  Ast.source_file_or_master I.env option ->
  Ast.source_file_or_master I.checkpoint ->
  Ast.source_file_or_master
val sedlex_with_menhir :
  (Sedlexing.lexbuf -> Parser.token) ->
  (string * Parser.token) list ->
  (Lexing.position -> Ast.source_file_or_master I.checkpoint) ->
  Sedlexing.lexbuf -> Ast.source_file_or_master
val parse_source_file :
  Utils.Pos.input_file -> Utils.Cli.frontend_lang -> Ast.program
val expand_includes :
  string ->
  Ast.program_item list ->
  Utils.Cli.frontend_lang -> Ast.program
val parse_top_level_file :
  Utils.Pos.input_file -> Utils.Cli.frontend_lang -> Ast.program
