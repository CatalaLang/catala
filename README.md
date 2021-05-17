<center>
<img src="https://github.com/CatalaLang/catala/raw/master/doc/images/logo.png" alt="Catala logo" width="100"/>
</center>

# Catala [![Catala chat][chat-image]][chat-link] ![CI][ci-link] ![Opam][opam-link] ![Licence][licence-link] ![Tag][tag-link] ![LoC][loc-link] ![Language][language-link] ![Issues][issues-link] ![Contributors][contributors-link] ![Activity][activity-link]

Catala is a domain-specific language for deriving
faithful-by-construction algorithms from legislative texts. To learn quickly
about the language and its features, you can jump right to the official
[Catala tutorial](https://catala-lang.org/en/examples/tutorial).
You can join the Catala community on [Zulip][chat-link]!

## Concepts

Catala is a programming language adapted for socio-fiscal legislative literate
programming. By annotating each line of the legislative text with its meaning
in terms of code, one can derive an implementation of complex socio-fiscal
mechanisms that enjoys a high level of assurance regarding the code-law
faithfulness.

Concretely, you have to first gather all the laws, executive orders, previous
cases, etc. that contain information about the socio-fiscal mechanism that
you want to implement. Then, you can proceed to annotate the text article by
article, in your favorite text editor :

<center>
<img src="https://github.com/CatalaLang/catala/raw/master/doc/images/ScreenShotVSCode.png" alt="Screenshot" height="450"/>
</center>

Once your code is complete and tested, you can use the Catala
compiler to produce a lawyer-readable PDF version of your
implementation. The Catala language has been specially designed
in collaboration with law professionals to ensure that the code
can be reviewed and certified correct by the domain experts, which
are in this case lawyers and not programmers.

<center>
<img src="https://github.com/CatalaLang/catala/raw/master/doc/images/CatalaScreenShot.png" alt="Screenshot" height="400"/>
</center>

The Catala language is special because its logical structure mimics
the logical structure of the law. Indeed, the core concept of
"definition-under-conditions" that builds on default logic has been formalized
by Professor Sarah Lawsky in her article
[A Logic for Statutes](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3088206).
The Catala language is the only programming language to our knowledge that
embeds default logic as a first-class feature, which is why it is the only
language perfectly adapted to literate legislative programming.

## Building and installation

Catala is available as an [opam package](https://opam.ocaml.org/packages/catala/)!
If opam is installed on your machine, simply execute:

    opam install catala

To get the cutting-edge, latest version of Catala, you
can also do

    opam pin add catala --dev-repo

However, if you wish to get the latest developments of the compiler, you probably
want to compile it from the sources of this repository. For that, see
[the dedicated readme](INSTALL.md).

## Usage

Use `catala --help` to get more information about the command line
options available.

The top-level CMakelists.txt contains definitions of targets that will be generated and then used. To generate them, call

    mkdir -p build/ && cmake -B build

Now, you can call e.g. `cmake --build build/ --target help` from the top directory or `make help` from build directory.

<!-- TODO: change make -> cmake --build build/ in some places -->


## Examples

See [the dedicated readme](examples/README.md).

## Contributing

See [the dedicated readme](CONTRIBUTING.md).

## Test suite

See [the dedicated readme](tests/README.md).

## Documentation

### Formal semantics

See [the dedicated readme](doc/formalization/README.md).

### Compiler documentation

The compiler documentation is auto-generated from its source code using
`dune` and `odoc`. Use

    make doc

to generate the documentation, then open the `doc/odoc.html` file in any browser.
The documentation is also accessible [online](https://catala-lang.org/ocaml_docs/).

## License

The library is released under the [Apache license (version 2)](LICENSE.txt).

## Limitations and disclaimer

Catala is a research project from Inria, the French National
Research Institute for Computer Science. The compiler is yet
unstable and lacks some of its features.

## Pierre Catala

The language is named after Pierre Catala, a professor of law who
pionneered the French legaltech by creating a computer database of law cases,
Juris-Data. The research group that he led in the late 1960s, the
Centre d’études et de traitement de l’information juridique (CETIJ),
has also influenced the creation by state conselor Lucien Mehl of the
Centre de recherches et développement en informatique juridique (CENIJ),
which eventually transformed into the entity managing the LegiFrance website,
acting as the public service of legislative documentation.

[chat-image]: https://img.shields.io/badge/zulip-join_chat-blue.svg?style=social&logo=zulip&color=5c75a2
[chat-link]: https://zulip.catala-lang.org/
[ci-link]: https://github.com/catalalang/catala/actions/workflows/build.yml/badge.svg
[licence-link]: https://img.shields.io/github/license/catalalang/catala
[tag-link]: https://img.shields.io/github/v/tag/catalalang/catala
[loc-link]: https://img.shields.io/tokei/lines/github/catalalang/catala
[issues-link]: https://img.shields.io/github/issues/catalalang/catala
[opam-link]: https://img.shields.io/badge/Package-opam-orange?logo=OCaml&link=https://opam.ocaml.org/packages/catala/
[language-link]: https://img.shields.io/github/languages/top/catalalang/catala
[contributors-link]: https://img.shields.io/github/contributors/catalalang/catala
[activity-link]: https://img.shields.io/github/commit-activity/m/catalalang/catala
