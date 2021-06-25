# French Law Libraries

This folder presents a working example of how Catala could be distributed and
deployed inside existing applications. Each sub-folder is specialized for
a particular programming language, and features a ready-to-use library of
all the French public algorithms coded up using Catala so far.

## General principles

Let us say you want to deploy a Catala program inside an application written
in programming language X. The Catala compiler will translate the source
Catala program into X, yielding a new `.x` source code file. This `.x` file
will export functions corresponding to the scopes of the original Catala
program. You can then reuse those exported functions in your application written
in X.

## OCaml

To see how to deploy Catala programs as an OCaml library, see
[the dedicated readme](ocaml/README.md).

## JS

To see how to deploy Catala programs as a JS library, see
[the dedicated readme](js/README.md).

## Python

To see how to deploy Catala programs as a Python library, see
[the dedicated readme](python/README.md).
