let () =
  let syntax_files = Array.to_list Sys.argv |> List.tl in
  if syntax_files = [] then failwith "Expected at least one syntax file";
  let lang_names =
    List.map
      (fun fname ->
        match
          Filename.chop_suffix_opt ~suffix:".json" (Filename.basename fname)
        with
        | None ->
          Printf.ksprintf failwith
            "Unexpected file: %s - expected a '<lang_name>.json' file" fname
        | Some n -> fname, n)
      syntax_files
  in
  let ocaml_module_rule (json_file, lang_name) =
    Printf.printf
      {dune|(rule
 (mode promote)
 (action
  (with-stdout-to %s.ml
   (pipe-stdout
    (run %%{exe:../gen/ocaml.exe} %%{dep:%s})
    (run ocamlformat - --impl)))))
|dune}
      lang_name json_file
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
      (String.concat " " (List.map snd lang_names))
  in
  List.iter
    (fun l ->
      ocaml_module_rule l;
      print_newline ())
    lang_names;
  languages_library ()
