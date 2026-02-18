(lang dune 3.13)

(name catala)

(generate_opam_files false)

(formatting)

(subst disabled)

(using menhir 3.0)

;;(using directory-targets 0.1)

(using dune_site 0.1)
(package
 (name catala)
 (sites (libexec plugins)))

(package
 (name catala-js))
