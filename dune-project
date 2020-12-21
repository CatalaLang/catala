(lang dune 2.2)
(name catala)
(version 0.2.0)
(generate_opam_files true)
(formatting)

(source (uri git+https://github.com/CatalaLang/catala.git))
(homepage https://github.com/CatalaLang/catala)
(bug_reports https://github.com/CatalaLang/catala/issues)
(authors "Denis Merigoux")
(maintainers "contact@catala-lang.org")
(license Apache2)


(package
 (name catala)
 (synopsis "Low-level language for tax code specification")
 (description "\| The Catala language is designed to be a low-level target for
              "\| higher-level specification languages for fiscal legislation.
 )
 (depends
   (ocaml (>= 4.07.0))
   (ANSITerminal (>= 0.8.2))
   (sedlex (>= 2.1))
   (menhir (>= 20200211))
   (menhirLib (>= 20200211))
   (unionFind (>= 20200320))
   (bindlib (>= 5.0.1))
   (dune-build-info (>= 2.0.1))
   (cmdliner (>= 1.0.4))
   (re (>= 1.9.0))
   (zarith (>= 1.10))
   (dune (and :build ))
   (ocamlgraph (>= 1.8.8))
   (odate (>= 0.6))
 )
)

(using menhir 2.1)
