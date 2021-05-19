# Contributing to Catala

The project is open to external contributions, in the spirit of open source.
If you want to open a pull request, please follow the instructions below.

To ask a question to the Catala team, please open an issue on this repository.
You can also join the [Zulip chat](https://zulip.catala-lang.org/) to ask
any questions about the project.

If you want to contribute to the project on a longer-term basis, or if you have
specific competences as a socio-fiscal lawyer or a programming language specialist,
please [contact the authors](mailto:contact@catala-lang.org).
The Catala team meets over visioconference once every two weeks.

Please note that the copyright of this code is owned by Inria;
by contributing, you disclaim all copyright interests in favor of Inria.
Both the code for the compiler and the examples in this repository are
distributed under the Apache2 license.

### Writing Catala code

Before writing Catala code, please read the
[tutorial](https://catala-lang.org/en/examples/tutorial). You can run the
programs of the tutorial yourself by following the instruction in the
[README of the `examples` directory](examples/README.md). Then, it is suggested
that you create a new example directory again according to the instructions of
this README.

Let us now present the typical Catala workflow. First, you need to locate
the legislative text that you want to use as a reference. Then, simply
copy-paste the text into your source file.

First you will have to format the copy-pasted text using Catala headings
and articles markers:

```markdown
## Heading

### Sub-heading (the more '#', the less important)

#### [Legislative atom]
```

Please look at the code of other examples to see how to format things properly.
While formatting the text, don't forget regularly to try and parse your example
using for instance

```
make -C examples/foo foo.tex
```

to see if you've made any syntax errors. Once the text formatting is done, you
can start to annotate each legislative atom (article, provision, etc.) with
some Catala code. To open up a code section in Catala, simply use

~~~markdown
```catala
# In code sections, comments start with #
scope Foo:
  <your code goes here>
```
~~~

While all the code sections are equivalent in terms of execution, you can
mark some as "metadata" so that they are printed differently on lawyer-facing
documents. Here's how it works:

~~~markdown
> Begin metadata # > Début métadonnées en français

```catala
declaration structure FooBar:
  data foo content boolean
  data bar content money

<your structure/enumeration/scope declarations goes here>
```

> End metadata # > Fin métadonnées en français
~~~

Again, make sure to regularly check that your example is parsing correctly. The error message from the compiler should help you debug the syntax if need be. You can also
live-test the programs you wrote by feeding them to the interpreter
(see the [README of the `examples` directory](examples/README.md)); this will
also type-check the programs, which is useful for debugging them.

## Working on the compiler

The Catala compiler is a standard dune-managed OCaml project.
You can look at the
[online OCaml documentation](https://catala-lang.org/ocaml_docs/) for the
different modules' interfaces as well as high-level architecture documentation.

Please note that the `ocamlformat` version this project uses is `0.18.0`.
Using another version may cause spurious diffs to appear in your pull requests.

### Example: adding a builtin function

The language provides a limited number of builtin functions, which are sometimes
needed for things that can't easily be expressed in Catala itself; in case you
need more, here is how one can be added:

- Choose a name wisely. Be ready to patch any code that already used the name
  for scope parameters, variables or structure fields, since it won't compile
  anymore.
- Add an element to the `builtin_expression` type in `surface/ast.ml(i)`
- Add your builtin in the `builtins` list in `surface/lexer.ml`, and with proper
  translations in all of the language-specific modules `surface/lexer_en.ml`,
  `surface/lexer_fr.ml`, etc.
- The rest can all be done by following the type errors downstream:
  - Add a corresponding element to the lower-level AST in `dcalc/ast.ml(i)`, type `unop`
  - Extend the translation accordingly in `surface/desugaring.ml`
  - Extend the printer (`dcalc/print.ml`) and the typer with correct type
    information (`dcalc/typing.ml`)
  - Finally, provide the implementations:
    - in `lcalc/to_ocaml.ml`, function `format_unop`
    - in `dcalc/interpreter.ml`, function `evaluate_operator`
- Update the syntax guide in `doc/syntax/syntax.tex` with your new builtin

## Internationalization

The Catala language should be adapted to any legislative text that follows a
general-to-specifics statutes order. Therefore, there exists multiple versions
of the Catala surface syntax, adapted to the language of the legislative text.

Currently, Catala supports English and French legislative text via the
`--language=en`, `--language=fr` or `--language=pl`  option.

Technically, support for new languages can be added via a new lexer. If you want
to add a new language, you can start from
[existing lexer examples](src/catala/catala_surface/lexer_fr.ml), tweak and open
a pull request. If you don't feel familiar enough with OCaml to do so, please
leave an issue on this repository.
