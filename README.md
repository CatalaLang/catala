# Catala

Catala is a domain-specific language for deriving
faithful-by-construction algorithms from legislative texts.

## Concepts

Catala is a programming language adapted for socio-fiscal legislative literate programming. By annotating each line of the
legislative text with its meaning in terms of code, one can derive
an implementation of complex socio-fiscal mechanisms that enjoys
a high level of assurance regarding the code-law faithfulness.

Concretely, you have to first gather all the laws, executive orders, previous cases, etc. that contain information about the
socio-fiscal mechanism that you want to implement. Then, you can
proceed to annotate the text article by article, in your favorite
text editor :

![Screenshot](https://github.com/CatalaLang/catala/raw/master/doc/ScreenShotAtom.png)

Once your code is complete and tested, you can use the Catala
compiler to produce a lawyer-readable PDF version of your
implementation. The Catala language has been specially designed
in collaboration with law professionals to ensure that the code
can be reviewed and certified correct by the domain experts, which
are in this case lawyers and not programmers.

![Screenshot](https://github.com/CatalaLang/catala/raw/master/doc/CatalaScreenShot.png)

The Catala language is special because its logical structure mimics
the logical structure of the law. Indeed, the core concept of
"definition-under-conditions" that builds on default logic has been formalized by Professor of Law Sarah Lawsky in her article [A Logic for Statutes](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3088206). The Catala language is the only
programming language to our knowledge that embeds default logic
as a first-class feature, which is why it is the only language
perfectly adapted to literate legislative programming.


## Catala motivating example : French "allocations familiales"

In the `example/allocations_familiales` folder, you will find the
`allocations_familiales.catala` file which contains the
algorithm computing French family benefits. The algorithm consists of annotations to the legislative
texts that define the family benetifs, using the literate programming paradigm. The Catala
compiler can extract from the `.catala` file a lawyer-readable version of the annotated text.

Currently, this lawyer-readable version comes in the form of a LaTeX document.
You will need to have a standard LaTeX distribution installed as well as the
`latexmk` build tool in order to enjoy the automated document generation process.

To get that lawyer-readable version (which is a LaTeX-created) PDF, simply use

    make allocations_familiales

from the repository root, once you have managed to install the
compiler (see bewlow). You can then open `examples/allocations_familiales/allocations_familiales.pdf`

## Limitations and disclaimer

### Early stage project

Catala is a research project from Inria, the French National
Research Institute for Computer Science. The compiler is yet very
unstable and lacks most of its features. Currently, it only
parses the surface language to producde the lawyer-readable PDF,
no interpreter or compiler backend is provided.

However, the language is bound to have a complete formal semantics
in the near future. This semantics will guide the compiler
implementation.

### Languages

Currently, the Catala language only enjoys a French surface
language, adapted to French law. However, it is perfectly
possible to craft new parsers for new surface languages adapted
to English, Spanish, etc. Contact the authors if you are interested.

## The Catala compiler

### OCaml requirements

The Catala compiler is written using OCaml. To install OCaml on your machine and
if you're running Linux ou MacOS, open a terminal and enter :

    ./install_opam.sh

This will install `opam`, the OCaml dependency manager and the
base OCaml compiler. If you're on Windows, the simplest solution
would be to use Cygwin or the Windows Subsystem for Linux.

Next, install all the build dependencies with

    make install-dependencies

This should ensure everything is set up for developping on Catala !


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

The `pigmentize` executable, used for instance by the `minted` LaTeX package,
will now point to the Catala-enabled version inside the virtual environment.

## Contributing

The project accepts pull requests. Please email the authors
if you are interested, whether you are a law professional, a
computer scientist or a developer. Please use this email address :

    denis DOT merigoux AT inria DOT fr

Please note that the copyright of this code is owned by Inria;
by contributing, you disclaim all copyright interests in favor of Inria.

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
