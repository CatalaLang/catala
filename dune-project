(lang dune 2.8)

(name catala)

(version 0.5.0)

(generate_opam_files true)

(formatting)

(source
 (uri git+https://github.com/CatalaLang/catala.git))

(homepage https://github.com/CatalaLang/catala)

(bug_reports https://github.com/CatalaLang/catala/issues)

(authors "Denis Merigoux, Nicolas Chataing, Emile Rolley")

(maintainers "contact@catala-lang.org")

(license Apache-2.0)

(package
 (name catala)
 (synopsis
  "Compiler and library for the literate programming language for tax code specification")
 (description
  "Catala is a domain-specific language for deriving faithful-by-construction\nalgorithms from legislative texts. See https://catala-lang.org for more information\n")
 (depends
  (ocaml
   (>= 4.11.0))
  (ANSITerminal
   (>= 0.8.2))
  (sedlex
   (>= 2.4))
  (menhir
   (>= 20200211))
  (menhirLib
   (>= 20200211))
  (unionFind
   (>= 20200320))
  (bindlib
   (>= 5.0.1))
  (cmdliner
   (= 1.0.4))
  (re
   (>= 1.9.0))
  (zarith
   (>= 1.12))
  (zarith_stubs_js
   (>= v0.14.1))
  (ocamlgraph
   (>= 1.8.8))
  (calendar
   (>= 2.04))
  (visitors
   (>= 20200210))
  (benchmark
   (>= 1.6))
  (js_of_ocaml-ppx
   (>= 3.8.0))
  (camomile
   (>= 1.0.2))
  (z3
   (>= 4.8.11))
  (cppo
   (>= 1))
  (obelisk :dev)
  (ocamlformat
   (and
    :dev
    (= 0.19.0)))))

(package
 (name french_law)
 (synopsis
  "A collection of algorithms and computations defined by French law")
 (description
  "This library contains the implementations of algorithmic portions of French
   law. The library source code was generated from Catala annotations of the
   relevant portions of the French law, see https://catala-lang.org")
 (depends
  (ocaml
   (>= 4.11.0))
  (catala
   (= :version))
  (conf-npm :dev)))

(package
 (name ninja_utils)
 (synopsis
  "A collection of utility functions used to generate Ninja build files")
 (description
  "This library contains the implementations of utility functions used to
   generate Ninja build files -- see https://ninja-build.org. It's currently
   used by the Catala build system (see
                                     https://github.com/CatalaLang/catala/tree/master/build_system)")
 (depends
  (ocaml
   (>= 4.11.0))
  (re
    (>= 1.10.3))
  (catala
   (= :version))))

(using menhir 2.1)
