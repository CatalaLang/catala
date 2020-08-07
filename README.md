
<center>
<img src="https://github.com/CatalaLang/catala/raw/master/doc/logo.png" alt="Catala logo" width="100"/>
</center>

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

<center>
<img src="https://github.com/CatalaLang/catala/raw/master/doc/ScreenShotAtom.png" alt="Screenshot" height="500"/>
</center>

Once your code is complete and tested, you can use the Catala
compiler to produce a lawyer-readable PDF version of your
implementation. The Catala language has been specially designed
in collaboration with law professionals to ensure that the code
can be reviewed and certified correct by the domain experts, which
are in this case lawyers and not programmers.

<center>
<img src="https://github.com/CatalaLang/catala/raw/master/doc/CatalaScreenShot.png" alt="Screenshot" height="500"/>
</center>

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

    make -C allocations_familiales allocations_familiales.pdf

from the repository root, once you have managed to install the
compiler using [the dedicated readme](INSTALL.md). You can then open `examples/allocations_familiales/allocations_familiales.pdf`

## Languages

The Catala language should be adapted to any legislative text that follows a general-to-specifics statutes order. Therefore, there exists  multiple versions of the Catala surface syntax, adapted to the language of the lefislative text.

Currently, Catala supports English and French legislations via the `--language=en` or `--language=fr` option. Contact the authors
if you are interested in adding support for another language.

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

## Installation

See [the dedicated readme](INSTALL.md).

## Test suite

See [the dedicated readme](tests/README.md).

## Contributing

See [the dedicated readme](CONTRIBUTING.md).

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
