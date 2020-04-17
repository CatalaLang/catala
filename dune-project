(lang dune 2.2)
(name catala)
(version 0.1.1)
(generate_opam_files true)

(source (uri git+https://gitlab.inria.fr/verifisc/catala.git))
(homepage https://gitlab.inria.fr/verifisc/catala)
(bug_reports https://gitlab.inria.fr/verifisc/catala/issues)
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
 )
)
(using menhir 2.1)
