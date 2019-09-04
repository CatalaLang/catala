(lang dune 1.10)
(name verifisc)
(version 1.0.2)
(generate_opam_files true)

(source (uri git+https://gitlab.inria.fr/verifisc/verifisc.git))
(homepage https://gitlab.inria.fr/verifisc/verifisc)
(bug_reports https://gitlab.inria.fr/verifisc/verifisc/issues)
(authors "Denis Merigoux")
(maintainers "denis.merigoux@inria.fr")
(license Apache2)


(package
 (name verifisc)
 (synopsis "Low-level language for tax code specification")
 (description "\| The Verifisc language is designed to be a low-level target for
              "\| higher-level specification languages for fiscal legislation.
 )
 (depends
   (ocaml (>= 4.07.0))
   (dune (and :build ))
 )
)
