open Locale

let langs = Languages.languages () |> List.map (fun (_, l) -> l)
let lang_codes = List.map (fun l -> l.code) langs

let surface_lib =
  Printf.sprintf
    {dune|(library
 (name localised_lexers)
 (public_name catala.localised_lexers)
 (library_flags -linkall)
 (libraries catala_utils menhirLib sedlex re zarith shared_ast)
 (modules (:standard \ parser parser_errors parser_state parser_driver print surface))
 (wrapped false)
 (preprocess
  (per_module
   ((pps sedlex.ppx)
    %s
))))|dune}
    (String.concat " " (List.map (fun c -> "lexer_" ^ c) lang_codes))

let action fmt lang =
  Format.fprintf fmt
    "@[<v>(write-file lexer_%s.mli \"include Lexer_common.LocalisedLexer\")@ \
     @[<v 1>(with-stdout-to@  lexer_%s.cppo.ml@ @[<h>(run \
     %%{exe:../../languages/gen/macros.exe} %s)@])@]@ @[<v 1>(with-stdout-to@  \
     lexer_%s.ml@ @[<h>(run %%{bin:cppo} lexer_%s.cppo.ml \
     %%{dep:lexer.cppo.ml})@])@]@]@]@\n"
    lang.code lang.code lang.code lang.code lang.code

let () =
  print_endline surface_lib;
  Format.printf
    {dune|
(rule
 (targets @[<h>%s@])
 (mode (promote (only %s)))
 (action
  (progn|dune}
    (List.map
       (fun c -> Format.sprintf "lexer_%s.ml lexer_%s.mli" c c)
       lang_codes
    |> String.concat " ")
    (List.map (fun c -> Format.sprintf "lexer_%s.mli" c) lang_codes
    |> String.concat " ");

  List.iter (Format.printf "@[<v 4>   %a@]" action) langs;
  print_endline ")))"
