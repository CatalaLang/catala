
<center>
<img src="https://github.com/CatalaLang/catala/raw/master/doc/images/logo.png" alt="Catala logo" width="100"/>
</center>

# Catala

Catala is a domain-specific language for deriving
faithful-by-construction algorithms from legislative texts. To learn quickly
about the language and its features, you can jump right to the official 
[Catala tutorial](https://catala-lang.org/en/examples/tutorial).

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
<img src="https://github.com/CatalaLang/catala/raw/master/doc/images/ScreenShotVSCode.png" alt="Screenshot" height="500"/>
</center>

Once your code is complete and tested, you can use the Catala
compiler to produce a lawyer-readable PDF version of your
implementation. The Catala language has been specially designed
in collaboration with law professionals to ensure that the code
can be reviewed and certified correct by the domain experts, which
are in this case lawyers and not programmers.

<center>
<img src="https://github.com/CatalaLang/catala/raw/master/doc/images/CatalaScreenShot.png" alt="Screenshot" height="500"/>
</center>

The Catala language is special because its logical structure mimics
the logical structure of the law. Indeed, the core concept of
"definition-under-conditions" that builds on default logic has been formalized 
by Professor of Law Sarah Lawsky in her article 
[A Logic for Statutes](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3088206). 
The Catala language is the only programming language to our knowledge that 
embeds default logic as a first-class feature, which is why it is the only 
language perfectly adapted to literate legislative programming.

## Installation

See [the dedicated readme](INSTALL.md).

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

See [the dedicated readme](src/README.md).

## License

The library is released under the [Apache license (version 2)](LICENSE.txt).

## Limitations and disclaimer

Catala is a research project from Inria, the French National
Research Institute for Computer Science. The compiler is yet 
unstable and lacks some of its features. Currently, here is the list
of existing features:

* Literate programming output to HTML or LaTeX
* Typechecker and interpreter for most of the language


## Pierre Catala

The language is named after Pierre Catala, a professor of law who
pionneered the French legaltech by creating a computer database of law cases,
Juris-Data. The research group that he led in the late 1960s, the
Centre d’études et de traitement de l’information juridique (CETIJ),
has also influenced the creation by state conselor Lucien Mehl of the
Centre de recherches et développement en informatique juridique (CENIJ),
which eventually transformed into the entity managing the LegiFrance website,
acting as the public service of legislative documentation.
