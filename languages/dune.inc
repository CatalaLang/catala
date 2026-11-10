(rule
 (action
  (with-stdout-to english.ml
   (pipe-stdout
    (run %{exe:gen/ocaml.exe} %{dep:english.json})
    (run ocamlformat - --impl)))))

(rule
 (action
  (with-stdout-to french.ml
   (pipe-stdout
    (run %{exe:gen/ocaml.exe} %{dep:french.json})
    (run ocamlformat - --impl)))))

(rule
 (action
  (with-stdout-to polish.ml
   (pipe-stdout
    (run %{exe:gen/ocaml.exe} %{dep:polish.json})
    (run ocamlformat - --impl)))))

(library
 (name languages)
 (public_name catala.languages)
 (libraries catala.runtime_ocaml re locale)
 (flags (:standard -linkall))
 (wrapped false)
 (modules languages english french polish))