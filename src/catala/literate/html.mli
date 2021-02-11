module A = Surface.Ast
module P = Printf
module R = Re.Pcre
module C = Utils.Cli
val pre_html : string -> string
val raise_failed_pygments : string -> int -> 'a
val wrap_html :
  string list ->
  string option ->
  Utils.Cli.backend_lang ->
  Format.formatter -> (Format.formatter -> unit) -> unit
val pygmentize_code :
  string Utils.Pos.marked -> C.backend_lang -> string option -> string
val law_article_item_to_html :
  string option ->
  C.backend_lang -> Format.formatter -> A.law_article_item -> unit
val law_structure_to_html :
  string option ->
  C.backend_lang -> Format.formatter -> A.law_structure -> unit
val program_item_to_html :
  string option ->
  C.backend_lang -> Format.formatter -> A.program_item -> unit
val ast_to_html :
  string option -> C.backend_lang -> Format.formatter -> A.program -> unit
