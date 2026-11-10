(library
 (name localised_lexers)
 (public_name catala.localised_lexers)
 (library_flags -linkall)
 (libraries catala_utils menhirLib sedlex re zarith shared_ast)
 (modules (:standard \ parser parser_errors parser_state parser_driver print surface))
 (wrapped false)
 (preprocess
  (per_module
   ((pps sedlex.ppx)
    lexer_en lexer_fr lexer_pl
))))

(rule
 (targets lexer_en.ml lexer_en.mli lexer_fr.ml lexer_fr.mli lexer_pl.ml lexer_pl.mli)
 (mode (promote (only lexer_en.mli lexer_fr.mli lexer_pl.mli)))
 (action
  (progn
   (write-file lexer_en.mli "include Lexer_common.LocalisedLexer")
   (with-stdout-to
     lexer_en.cppo.ml
    (run %{exe:../../languages/gen/macros.exe} %{dep:../../languages/english.json}))
   (with-stdout-to
     lexer_en.ml
    (run %{bin:cppo} lexer_en.cppo.ml %{dep:lexer.cppo.ml}))
   (write-file lexer_fr.mli "include Lexer_common.LocalisedLexer")
   (with-stdout-to
     lexer_fr.cppo.ml
    (run %{exe:../../languages/gen/macros.exe} %{dep:../../languages/french.json}))
   (with-stdout-to
     lexer_fr.ml
    (run %{bin:cppo} lexer_fr.cppo.ml %{dep:lexer.cppo.ml}))
   (write-file lexer_pl.mli "include Lexer_common.LocalisedLexer")
   (with-stdout-to
     lexer_pl.cppo.ml
    (run %{exe:../../languages/gen/macros.exe} %{dep:../../languages/polish.json}))
   (with-stdout-to
     lexer_pl.ml
    (run %{bin:cppo} lexer_pl.cppo.ml %{dep:lexer.cppo.ml}))
)))
