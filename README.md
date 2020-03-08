# Lawspec

Lawspec is a domain-specific language for deriving faithful-by-construction algorithms
from legislative texts.

## Installation

The project is distributed as a Dune artifact. Use standard dune commands to build
and install the library. In particular, if you want to install the library as an opam
package, use the following command at the root of the repository:

    opam install ./

You can then can the compiler using the `lawspec` command.

## Usage

Use `lawspec --help` to get more information about the command line options available.

## Test

In the `test` folder, you will find the `allocations_familiales.lsp` file which contains the
algorithm computing French family benefits. The algorithm consists of annotations to the legislative
texts that define the family benetifs, using the literate programming paradigm. The `lawspec`
compiler can extract from the `.lsp` file a lawyer-readable version of the annotated text.
To get that lawyer-readable version (which is a LaTeX-created) PDF, use `make test` at the root of
the repository, and then use `make` inside ghe `test` directory to compile the LaTeX file.

## License

The library is released under the Apache license (version 2).
