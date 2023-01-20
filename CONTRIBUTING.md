# Contributing to Catala

The project is open to external contributions, in the spirit of open source.
If you want to open a pull request, please follow the instructions below.

To ask a question to the Catala team, please open an issue on this repository.
You can also join the [Zulip chat](https://zulip.catala-lang.org/) to ask
any questions about the project.

If you want to contribute to the project on a longer-term basis, or if you have
specific competences as a socio-fiscal lawyer or a programming language specialist,
please [contact the authors](mailto:contact@catala-lang.org).
The Catala team meets over visioconference once every week.

Please note that the copyright of this code is owned by Inria;
by contributing, you disclaim all copyright interests in favor of Inria.
Both the code for the compiler and the examples in this repository are
distributed under the Apache2 license.

## Writing Catala code

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

#### Legislative atom
```

Please look at the code of other examples to see how to format things properly.
While formatting the text, don't forget regularly to try and parse your example
using for instance

```
make -C examples/foo foo.tex
make -C examples/foo foo.py
make -C examples/foo foo.ml
```

to see if you've made any syntax errors. Once the text formatting is done, you
can start to annotate each legislative atom (article, provision, etc.) with
some Catala code. To open up a code section in Catala, simply use

````markdown
```catala
# In code sections, comments start with #
scope Foo:
  <your code goes here>
```
````

While all the code sections are equivalent in terms of execution, you can
mark some as "metadata" so that they are printed differently on lawyer-facing
documents. Here's how it works:

````markdown
```catala-metadata
declaration structure FooBar:
  data foo content boolean
  data bar content money

<your structure/enumeration/scope declarations goes here>
```
````

Again, make sure to regularly check that your example is parsing correctly. The error message from the compiler should help you debug the syntax if need be. You can also
live-test the programs you wrote by feeding them to the interpreter
(see the [README of the `examples` directory](examples/README.md)); this will
also type-check the programs, which is useful for debugging them.

## Working on the compiler

The Catala compiler is a standard dune-managed OCaml project.
You can look at the
[online OCaml documentation](https://catala-lang.org/ocaml_docs/) for the
different modules' interfaces as well as high-level architecture documentation.

### Installing and using nix

We provide an nix environement to develop the Catala compiler. It is available
after [installing nix](https://nixos.org/download.html). You can then just
use `nix develop` to enter the environment.

### Example: adding a builtin function

The language provides a limited number of builtin functions, which are sometimes
needed for things that can't easily be expressed in Catala itself; in case you
need more, here is how one can be added:

- Choose a name wisely. Be ready to patch any code that already used the name
  for scope parameters, variables or structure fields, since it won't compile
  anymore.
- Add an element to the `builtin_expression` type in `surface/ast.ml`
- Add your builtin in the `builtins` list in `surface/lexer.cppo.ml`, and with
  proper translations in all of the language-specific modules
  `surface/lexer_en.cppo.ml`, `surface/lexer_fr.cppo.ml`, etc. Don't forget the
  macro at the beginning of `lexer.cppo.ml`.
- The rest can all be done by following the type errors downstream:
  - Add a corresponding element to the lower-level AST in `shared_ast/definitions.ml`, type `Op.t`
  - Extend the generic operations on operators in `shared_ast/operators.ml` as well as the type information for the operator
  - Extend the translation accordingly in `desugared/from_surface.ml`
  - Extend the printer (`shared_ast/print.ml`)
  - Finally, provide the implementations:
    - in `dcalc/interpreter.ml`, function `evaluate_operator`
    - in `../runtimes/ocaml/runtime.ml`
    - in `../runtimes/python/catala/src/catala/runtime.py`
- Update the syntax guide in `doc/syntax/syntax.tex` with your new builtin

### Internationalization of the Catala syntax

The Catala language should be adapted to any legislative text that follows a
general-to-specifics statutes order. Therefore, there exists multiple versions
of the Catala surface syntax, adapted to the language of the legislative text.

Currently, Catala supports English, French and Polish legislative text via the
`--language=en`, `--language=fr` or `--language=pl` options.

To add support for a new language:

- the basic syntax localisation is defined in
  `compiler/surface/lexer_xx.cppo.ml` where `xx` is the language code (`en`,
  `fr`...)
- copy the files from another language, e.g.
  [english](compiler/surface/lexer_en.cppo.ml), then replace the strings with your
  translations. Be careful with the following:

  - The file must be encoded in latin-1
  - For a given token `FOO`, define `MS_FOO` to be the string version of the
    keyword. Due to the encoding, use `\xNN` [escape
    sequences](https://ocaml.org/manual/lex.html#escape-sequence) for utf8
    characters.
  - If the string contains spaces or non-latin1 characters, you need to define
    `MR_FOO` as well with a regular expression in [sedlex
    format](https://github.com/ocaml-community/sedlex#lexer-specifications).
    Replace spaces with `", space_plus, "`, and unicode characters with `", 0xNNNN, "` where `NNNN` is the hexadecimal unicode codepoint.

  **Hint:** You may get syntax errors with unhelpful locations because of
  `sedlex`. In that case the command `ocamlc _build/default/compiler/surface/lexer_xx.ml` may point you to the source of the
  error.

- add your translation to the compilation rules:
  - in `compiler/surface/dune`, copying another `parser_xx.cppo.ml` rule
  - in the `extensions` list in `compiler/driver.ml`
  - add a corresponding variant to `compiler/utils/cli.ml` `backend_lang`, try
    to run `make build` and follow all type errors and `match non exhaustive`
    warnings to be sure it is well handled everywhere.
- you may want to add syntax highlighting support, see `syntax_highlighting/`
  and the rules in `Makefile`
- add examples and documentation!

Feel free to open a pull request for discussion even if you couldn't go through
all these steps, the `lexer_xx.cppo.ml` file is the important part.

### Example: writing custom backends as plugins

Catala has support for dynamically-loaded plugins to use as alternative
backends. See `compiler/plugins` for examples, and [the
documentation](https://catala-lang.org/ocaml_docs/catala/plugins.html) for more
detail.

### Automatic formatting

Please ensure to submit commits formatted using the included `ocamlformat`
configuration. The `make build` target should ensure that.

In case the formatting rules or ocamlformat version changed remotely, you can
use [this script](https://gist.github.com/AltGr/2891a61f721c8fd85b1da71e10c691b6) to
reformat your branch patch by patch before rebasing.

### Hand-updating packages in the nix part

Requirements of catala that are not inside [nixpkgs](https://github.com/nixos/nixpkgs) are available inside the `.nix` directory of the repo. The main part is inside the `.nix/packages.nix`, where all the packages are either added (because absent from nixpkgs) using `ocamlPackage.callPackage`; or modified from nixpkgs, for instance cmdliner is currently pinned at version 1.1.0.

