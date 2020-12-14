# Contributing to Catala

The project is open to external contributions, in the spirit of open source. 
If you want to open a pull request, please follow the instructions below.

To ask a question to the Catala team, please open an issue on this repository. 
If you want to contribute to the project on a longer-term basis, or if you have 
specific competences as a socio-fiscal lawyer or a programming language specialist, 
please [contact the authors](mailto:contact@catala-lang.org).
The Catala team meets over visioconference once every two weeks.

Please note that the copyright of this code is owned by Inria;
by contributing, you disclaim all copyright interests in favor of Inria. 
Both the code for the compiler and the examples in this repository are 
distributed under the Apache2 license.

## Writing Catala examples

### Setting up the machinery

This section describes what to do to setup a working directory for a new Catala example, as well as the development cycle. Let us suppose that you want to create a new example named `foo`.

First, follow the instructions of [the installation readme](INSTALL.md) to get the compiler up and working up to `make build`. You can also set up the syntax highlighting for your editor.

Then, create the directory `examples/foo`. In there, create a master source file `foo.catala` that will be the root of your Catala program. You can then start programming in `foo.catala`, or split up your example into multiple files. In the later case, `foo.catala` must only contain something like this:

```
@@Master file@@

@@Include: bar.catala@@
```

where `examples/bar.catala` is another source file containing code for your example. Make sure you start by including some content in the source files, like

```
Hello, world!
```

Now let's build the example, create a `Makefile` with the following contents:

```Makefile
CATALA_LANG=en # or fr if your source code is in French
SRC=foo.catala

include ../Makefile.common
```

The `include` creates automatically all the targets you will need for your example. For instance, after making sure the compiler is built, you can launch

```
make -C examples/foo foo.tex
```

from the repository root to create the LaTeX weaving output of your source program. `Hello, world!` should appear in there.

Finally, please add a rule for your example in the repository root `Makefile` in the section "Examples-related rules", following the pattern for other examples. This will ensure that
your example is built every time the compiler is modified; if a change in the compiler breaks your example, the authors will be notified and find a solution.

### Writing Catala code

Let us now present the typical Catala workflow. First, you need to locate the legislative text that you want to use as a reference. Then, simply copy-paste the text into your source file.

First you will have to format the copy-pasted text using Catala headings and articles markers:

```
@@Heading@@

@@Sub-heading (the more +, the less important)@@++

@Legislative atom@
```

Please look at the code of other examples to see how to format things properly. While formatting the text, don't forget regularly to try and parse your example using for instance


```
make -C examples/foo foo.tex
```

to see if you've made any syntax errors. Once the text formatting is done, you can start to annotate each legislative atom (article, provision, etc.) with some Catala code. To open up a code section in Catala, simply use

```
/*
# In code sections, comments start with #
scope Foo:
  <your code goes here>
*/
```

While all the code sections are equivalent in terms of execution, you can mark some as "metadata" so that they are printed differently on lawyer-facing documents. Here's how it works:

```
@@Begin metadata@@ # @@Début métadonnées@@ en français
/*
declaration structure FooBar:
  data foo content boolean
  data bar content money

<your structure/enumeration/scope declarations goes here>
*/
@@End metadata@@ # @@Fin métadonnées@@ en français
```

Again, make sure to regularly check that your example is parsing correctly. The error message from the compiler should help you debug the syntax if need be.

## Working on the compiler

The Catala compiler is a standard dune-managed OCaml project. You can look at the [online OCaml documentation](https://catala-lang.org/ocaml_docs/) for the different modules' interfaces.

## Internationalization

The Catala language should be adapted to any legislative text that follows a 
general-to-specifics statutes order. Therefore, there exists  multiple versions 
of the Catala surface syntax, adapted to the language of the legislative text.

Currently, Catala supports English and French legislative text via the 
`--language=en` or `--language=fr` option. 

Technically, support for new languages can be added via a new lexer. If you want 
to add a new language, you can start from 
[existing lexer examples](src/catala/catala_surface/lexer_fr.ml), tweak and open 
a pull request. If you don't feel familiar enough with OCaml to do so, please 
leave an issue on this repository.
