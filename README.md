<div align="center">
  <img src="https://github.com/CatalaLang/catala/raw/master/doc/images/logo.png" alt="Catala logo" width="120"/>
  <h3 align="center">
	<big>Catala</big>
  </h3>
  <p align="center">
   <a href="https://book.catala-lang.org/"><strong>Explore the docs »</strong></a>
   <br/>
   <a href="https://book.catala-lang.org/1-0-getting_started.html">Getting started</a>
   •
   <a href="https://github.com/CatalaLang/catala/issues">Report Bug</a>
   •
   <a href="https://github.com/CatalaLang/catala/blob/master/CONTRIBUTING.md">Contribute</a>
   •
   <a href="https://zulip.catala-lang.org/">Join Zulip Chat</a>
  </p>

![CI][ci-link] [![Opam][opam-link]](https://opam.ocaml.org/packages/catala/) [![Licence][licence-link]](https://www.apache.org/licenses/LICENSE-2.0) ![Tag][tag-link] ![Language][language-link] [![Issues][issues-link]](https://github.com/CatalaLang/catala/issues) [![Contributors][contributors-link]](https://github.com/CatalaLang/catala/graphs/contributors) [![Activity][activity-link]](https://github.com/CatalaLang/catala/pulse)

Catala is a domain-specific language for deriving
faithful-by-construction algorithms from legislative texts. To learn quickly
about the language and its features, you can jump right to the official
[Catala tutorial](https://catala-lang.org/en/examples/tutorial).
You can join the Catala community on [Zulip][chat-link]!

</div>

<br>

<details>
  <summary>Table of Contents</summary>

<!-- vim-markdown-toc GitLab -->

* [Concepts](#concepts)
* [The Catala book](#the-catala-book)
* [IDE Extensions and Tooling](#ide-extensions-and-tooling)
  * [Syntax highlighting](#syntax-highlighting)
  * [Catala VScode extension](#catala-vscode-extension)
  * [Code formatting](#code-formatting)
* [Documentation](#documentation)
  * [Syntax cheat sheet](#syntax-cheat-sheet)
  * [Formal semantics](#formal-semantics)
  * [Compiler documentation](#compiler-documentation)
* [Examples](#examples)
* [API](#api)
* [Contributing](#contributing)
* [Test suite](#test-suite)
* [License](#license)
* [Limitations and disclaimer](#limitations-and-disclaimer)
* [Pierre Catala](#pierre-catala)

<!-- vim-markdown-toc -->

</details>

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

<div align="center">
<img src="https://github.com/CatalaLang/catala/raw/master/doc/images/ScreenShotVSCode.png" alt="Screenshot" height="350"/>
</div>

Once your code is complete and tested, you can use the Catala
compiler to produce a lawyer-readable PDF version of your
implementation. The Catala language has been specially designed
in collaboration with law professionals to ensure that the code
can be reviewed and certified correct by the domain experts, which
are in this case lawyers and not programmers.

<div align="center">
<img src="https://github.com/CatalaLang/catala/raw/master/doc/images/CatalaScreenShot.png" alt="Screenshot" height="350"/>
</div>

The Catala language is special because its logical structure mimics
the logical structure of the law. Indeed, the core concept of
"definition-under-conditions" that builds on default logic has been formalized
by Professor Sarah Lawsky in her article
[A Logic for Statutes](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3088206).
The Catala language is the only programming language to our knowledge that
embeds default logic as a first-class feature, which is why it is the only
language perfectly adapted to literate legislative programming.

## The Catala Book

The one-stop-shop for tutorials, installation instructions, FAQ and reference
guide about Catala is the [Catala book](https://book.catala-lang.org) available
at:

<div align="center">
<a href="https://book.catala-lang.org">https://book.catala-lang.org</a>
</div>

## IDE Extensions and Tooling

### Syntax highlighting

Syntax highlighting is available for several text-editors. Scripts can
be found
[here](https://github.com/CatalaLang/catala/tree/master/syntax_highlighting).

### Catala VSCode Extension

A VSCode extension for Catala is available [on the
marketplace](https://marketplace.visualstudio.com/items?itemName=catalalang.catala).
It bundles a syntax highlighter and a dedicated LSP server which
offers support for code navigation, auto-completion along with a UX
for test suites. See the [dedicated
repository](https://github.com/catalaLang/catala-language-server) for
more details.

### Code formatting

A code formatting tool, `catala-format` is available alongside the LSP
server. If installed, code formatting is directly available in VSCode.
This tool is based on a [`tree-sitter`
grammar](https://github.com/CatalaLang/tree-sitter-catala) for
Catala. See the [dedicated
repository](https://github.com/catalaLang/catala-format) for more
details.

## Additional documentation items

### Syntax cheat sheet

A complete and handy reference of the Catala syntax can be found in
the [cheat sheet](https://catalalang.github.io/catala/syntax.pdf) (for
French and English versions of the syntax).

### Formal semantics

To audit the formal proof of the partial certification of the Catala compiler,
see [the dedicated readme](doc/formalization/README.md).

### Compiler documentation

The documentation is accessible online, both for the [latest release](https://catala-lang.org/ocaml_docs/) and [bleeding-edge version](https://catalalang.github.io/catala/api-doc/).


It is otherwise generated from the compiler source code using
`dune` and `odoc`. Run

    make doc

to generate the documentation, then open the `doc/odoc.html` file in any browser.

## Examples

To explore the different programs written in Catala, see [the dedicated
readme](https://github.com/CatalaLang/catala-examples/blob/master/README.md).

## API

To know how to use the code generated by the Catala compiler in your favorite
programming language, head to the [readme of the French law
library](https://github.com/CatalaLang/french-law/blob/master/README.md). The
corresponding pre-built examples are also
[available](https://catalalang.github.io/catala/).

## Contributing

To know how you can contribute to the project, see
[the dedicated readme](CONTRIBUTING.md).

## Test suite

To know how to run or improve the Catala reference test suite,
see [the dedicated readme](tests/README.md).

## License

The compiler and all the code contained in this repository is released under
the [Apache license (version 2)](LICENSE.txt) unless another license is explicited
for a sub-directory.

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
[ci-link]: https://github.com/catalalang/catala/actions/workflows/harness.yml/badge.svg
[licence-link]: https://img.shields.io/github/license/catalalang/catala
[tag-link]: https://img.shields.io/github/v/tag/catalalang/catala
[loc-link]: https://img.shields.io/tokei/lines/github/catalalang/catala
[issues-link]: https://img.shields.io/github/issues/catalalang/catala
[opam-link]: https://img.shields.io/badge/Package-opam-orange?logo=OCaml&link=https://opam.ocaml.org/packages/catala/
[language-link]: https://img.shields.io/github/languages/top/catalalang/catala
[contributors-link]: https://img.shields.io/github/contributors/catalalang/catala
[activity-link]: https://img.shields.io/github/commit-activity/m/catalalang/catala
