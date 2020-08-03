(lang dune 2.2)
(name catala)
(version 0.1.1)
(generate_opam_files true)

(source (uri git+https://github.com/CatalaLang/catala.git))
(homepage https://github.com/CatalaLang/catala)
(bug_reports https://github.com/CatalaLang/catala/issues)
(authors "Denis Merigoux")
(maintainers "denis.merigoux@inria.fr")
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
   (dune-build-info (>= 2.0.1))
   (cmdliner (>= 1.0.4))
   (re (>= 1.9.0))
   (dune (and :build ))
   (ocamlgraph (>= 1.8.8))
 )
)

(package
 (name legifrance_catala)
 (synopsis "Interaction tool between French Catala programs and the LegiFrance API")
 (depends
   (ocaml (>= 4.07.0))
   (ANSITerminal (>= 0.8.2))
   (sedlex (>= 2.1))
   (menhir (>= 20200211))
   (menhirLib (>= 20200211))
   (dune-build-info (>= 2.0.1))
   (cmdliner (>= 1.0.4))
   (re (>= 1.9.0))
   (dune (and :build ))
   (reason ( >= 3.6.0))
   (yojson ( >= 1.7.0))
   (cohttp-lwt-unix ( >= 2.5.1))
   (lwt ( >= 5.3.0))
   (cohttp ( >= 2.5.1))
   (tls ( >= 0.11.1))
 )
)

(using menhir 2.1)
