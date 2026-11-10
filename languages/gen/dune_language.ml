let () =
  let syntax_files = Array.to_list Sys.argv |> List.tl in
  if syntax_files = [] then failwith "Expected at least one syntax file";
  let lang_names =
    List.map
      (fun fname ->
        match Filename.chop_suffix_opt ~suffix:".json" fname with
        | None ->
          Printf.ksprintf failwith
            "Unexpected file: %s - expected a '<lang_name>.json' file" fname
        | Some n -> n)
      syntax_files
  in
  let ocaml_module_rule lang_name =
    Printf.printf
      {dune|(rule
 (action
  (with-stdout-to %s.ml
   (pipe-stdout
    (run %%{exe:gen/ocaml.exe} %%{dep:%s.json})
    (run ocamlformat - --impl)))))
|dune}
      lang_name lang_name
  in
  let languages_library () =
    Printf.printf
      {dune|(library
 (name languages)
 (public_name catala.languages)
 (libraries catala.runtime_ocaml re locale)
 (flags (:standard -linkall))
 (wrapped false)
 (modules languages %s))|dune}
      (String.concat " " lang_names)
  in
  List.iter
    (fun l ->
      ocaml_module_rule l;
      print_newline ())
    lang_names;
  languages_library ()
