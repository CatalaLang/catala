# A date calculation library

## Aim

This library handles dates (`YYYY-MM-DD`) and periods (in days, months and years). It provides operators on dates and periods. The addition of dates and periods containing months or years is a tricky case that may require roundings. We have taken special care to define those rounding operators and expose different rounding modes for users. 

This library is a work in progress. You can find the library's description in `lib/dates.mli`. There are also a Python and C implementations (which correspond to ports of the OCaml implementation).

The full semantics of the library has been formalized and is available in the related ESOP 2024 paper [Formalizing Date Arithmetic and Statically Detecting Ambiguities for the Law](https://hal.science/hal-04536403).

## Installation

Just run `opam install dates_calc` or `opam install .` if you've cloned the git repository.

## Building the documentation

The documentation can be built with `dune build @doc`, and is then available in `doc/odoc.html`.
