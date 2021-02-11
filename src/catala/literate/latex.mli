module A = Surface.Ast
module R = Re.Pcre
module C = Utils.Cli
val pre_latexify : string -> string
val wrap_latex :
  string list ->
  string option ->
  C.backend_lang -> Format.formatter -> (Format.formatter -> unit) -> unit
val math_syms_replace : string -> string
val law_article_item_to_latex :
  C.backend_lang -> Format.formatter -> A.law_article_item -> unit
val law_structure_to_latex :
  C.backend_lang -> Format.formatter -> A.law_structure -> unit
val program_item_to_latex :
  C.backend_lang -> Format.formatter -> A.program_item -> unit
val ast_to_latex : C.backend_lang -> Format.formatter -> A.program -> unit
