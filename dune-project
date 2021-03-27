(lang dune 2.2)

(name catala)
(version 0.3.0)
(generate_opam_files true)
(formatting)

(source (uri git+https://github.com/CatalaLang/catala.git))
(homepage https://github.com/CatalaLang/catala)
(bug_reports https://github.com/CatalaLang/catala/issues)
(authors "Denis Merigoux")
(maintainers "contact@catala-lang.org")
(license Apache-2.0)


(package
 (name catala)
 (synopsis "Literate programming language for tax code specification")
 (description "\| Catala is a domain-specific language for deriving faithful-by-construction
              "\| algorithms from legislative texts. See https://catala-lang.org for more information
 )
 (depends
   (ocaml (>= 4.08.0))
   (ANSITerminal (>= 0.8.2))
   (sedlex (>= 2.1))
   (menhir (>= 20200211))
   (menhirLib (>= 20200211))
   (unionFind (>= 20200320))
   (bindlib (>= 5.0.1))
   (cmdliner (>= 1.0.4))
   (re (>= 1.9.0))
   (zarith (>= 1.10))
   (zarith_stubs_js (>= 0.14.0))
   (dune (>= 2.2))
   (ocamlgraph (>= 1.8.8))
   (calendar (>= 2.04))
   (visitors (>= 20200210))
   (benchmark (>= 1.6))
   (js_of_ocaml-ppx (>= 3.8.0))
   (camomile (>= 1.0.2))
 )
)

(using menhir 2.1)
(allow_approximate_merlin)