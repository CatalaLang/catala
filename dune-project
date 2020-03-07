(lang dune 1.10)
(name lawspec)
(version 1.0.2)
(generate_opam_files true)

(source (uri git+https://gitlab.inria.fr/verifisc/lawspec.git))
(homepage https://gitlab.inria.fr/verifisc/lawspec)
(bug_reports https://gitlab.inria.fr/verifisc/lawspec/issues)
(authors "Denis Merigoux")
(maintainers "denis.merigoux@inria.fr")
(license Apache2)


(package
 (name lawspec)
 (synopsis "Low-level language for tax code specification")
 (description "\| The Lawspec language is designed to be a low-level target for
              "\| higher-level specification languages for fiscal legislation.
 )
 (depends
   (ocaml (>= 4.07.0))
   (ANSITerminal (>= 0.8.2))
   (sedlex (>= 2.1))
   (menhirLib (>= 20200211))
   (dune (and :build ))
 )
)
(using menhir 2.1)
