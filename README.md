# Catala

Catala is a domain-specific language for deriving faithful-by-construction algorithms
from legislative texts.

## OCaml requirements

The Catala compiler is written using OCaml. To install OCaml on your machine and
if you're running Linux ou MacOS, open a terminal and enter :

    ./install_opam.sh

This will install `opam`, the OCaml dependency manager and the base OCaml compiler.
Next, install all the build dependencies with

    make install-dependencies

This should ensure everything is set up for developping on Catala !

## The Catala compiler

### Installation

The project is distributed as a Dune artifact. Use standard dune commands to build
and install the library. In particular, if you want to install the library as an opam
package, use the following command at the root of the repository:

    opam install ./

You can then can the compiler using the `catala` command.

### Usage

Use `catala --help` to get more information about the command line options available.

## Syntax highlighting

The Catala language also comes with syntax highlighting to
ease program development. The syntax highlighting is done
with the [Iro](https://eeyo.io/iro/) compiler that allows
writing the syntax only once, and then export it to formats
understood by various IDE. Currently, two syntax
highlighting plugins are under version control.

### Atom

To get Catala syntax highlighting in Atom, simply enter from
the root of the repository :

    ln -s -f $(pwd)/syntax_highlighting/atom ~/.atom/packages

You can now reload Atom and check that you have syntax highlighting on any `.catala` file.

### Pygments

Pygments is a Python-based versatile lexer for various
programming languages. To use a version of Pygments
augmented with the Catala plugin, you need to execute the
script `syntax_highlighting/pygments/set_up_pygments.sh`

This script assumes a `python3` executable on tour machine,
as well as the `virtualenv` package which you can install
using `python3 -m pip install virtualenv` .

The scripts sets up a virtual environement in `syntax_highlighting/pygments/pygments/env`, which will
contain the modified version of Pygments that has Catala
support. You can use this virtual environnement with

    source syntax_highlighting/pygments/pygments/env/bin/activate

## Catala motivating example : French "allocations familiales"

In the `example/allocations_familiales` folder, you will find the
`allocations_familiales.catala` file which contains the
algorithm computing French family benefits. The algorithm consists of annotations to the legislative
texts that define the family benetifs, using the literate programming paradigm. The `catala`
compiler can extract from the `.catala` file a lawyer-readable version of the annotated text.

To get that lawyer-readable version (which is a LaTeX-created) PDF, simply use

    make allocations_familiales

from the repository root. You can then open `examples/allocations_familiales/allocations_familiales.pdf`


## License

The library is released under the Apache license (version 2).

## Pierre Catala

The language is named after Pierre Catala, a professor of law who
pionneered the French legaltech by creating a computer database of law cases,
Juris-Data. The research group that he led in the late 1960s, the
Centre d’études et de traitement de l’information juridique (CETIJ),
has also influenced the creation by state conselor Lucien Mehl of the
Centre de recherches et développement en informatique juridique (CENIJ),
which eventually transformed into the entity managing the LegiFrance website,
acting as the public service of legislative documentation.
