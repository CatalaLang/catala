# Catala examples

This directory contains examples of Catala programs. It is highly recommended
to locate your own Catala programs in this directory, since programs in this
directory will receive first-class support during the alpha and beta stage
of the Catala programming language development.

## List of examples

- `allocations_familiales/`: computation of the French family benefits, based
  on the _Code de la sécurité sociale_. This case study is the biggest and
  most ambitious for Catala so far.
- `code_general_impots/`: computation of the French income tax, based on the
  _Code général des impôts_. Currently, there are only stubs of program.
- `tutorial/`: Catala language tutorial for developers of tech-savvy lawyers.
  The tutorial is written like a piece of legislation that gets annotated by
  Catala snippets.
- `us_tax_code/`: contains the Catala formalization of several sections of the
  US Tax Code.

## Building and running examples

Building and running examples is done via Makefiles. Each example directory
contains its own Makefile, which includes `Makefile.common.mk`. This common
Makefiles defines a list of targets that call the Catala compiler with the
right options. Each of these targers can be called from the root of the
repository with:

        make -C examples/<directory of example> <name of target>

The `<name of target>` can be replaced with the following (we assume an example
file `examples/foo/foo.catala_en`) list.

- `foo.run`: interprets the Catala program contained in `foo.catala_en`. Note
  that you have to pass in the scope that you want to interpret via the `SCOPE`
  Makefile variable (`SCOPE=FooScope make -C examples/foo foo.run`).
- `foo.tex`: builds the LaTeX literate programming output from the Catala program
- `foo.pdf`: compiles `foo.tex` using `latexmk`
- `foo.html`: builds the HTML literate programming output from the Catala program

When invoking any of these targets, additional options to the Catala compiler
can be passed using the `CATALA_OPTS` Makefile variable.

## Testing examples

Unit testing is important, and we encourage Catala developers to write lots
of tests for their programs. Again, the Makefile system provides a way to
collect tests into a regression test suite.

In order to enjoy the benefits of this system, you have to create a `tests/`
directory in your examples directory, for instance `examples/foo/tests`. Then,
create a test file `foo_tests.catala_en` inside that directory.

Inside `foo_tests.catala_en`, declare one ore more test scopes. It is assumed
that all these scopes should execute correctly. Include the program scope you
want to test and use assertions to provide the expected values of your test.
See existing tests in examples directory for more information.

Once your tests are written, then will automatically be added to the regression
suite executed using

    make -C examples tests

You can isolate a part of the regression suite by invoking:

    TEST_FILES=examples/tests/foo/foo_tests.catala_en make -C examples tests

## Adding an example

This section describes what to do to setup a working directory for a new Catala
example, as well as the development cycle. Let us suppose that you want to
create a new example named `foo`.

First, follow the instructions of [the installation readme](../INSTALL.md) to
get the compiler up and working up to `make build`. You can also set up the
syntax highlighting for your editor.

Then, create the directory `examples/foo`. In there, create a master source
file `foo.catala` that will be the root of your Catala program.
You can then start programming in `foo.catala`, or split up your example
into multiple files. In the later case, `foo.catala` must only contain
something like this:

```markdown
# Master file

> Include: bar.catala
```

where `examples/bar.catala` is another source file containing code for your
example. Make sure you start by including some content in the source files,
like

```
Hello, world!
```

To build and run the example, create a `Makefile` in `foo/`
with the following contents:

```Makefile
CATALA_LANG=en # or fr if your source code is in French
SRC=foo.catala

include ../Makefile.common.mk
```

The `include` creates automatically all the targets you will need for your example. For instance, after making sure the compiler is built, you can launch

```
make -C examples/foo foo.tex
```

from the repository root to create the LaTeX weaving output of your source
program. `Hello, world!` should appear in `examples/foo/foo.tex`.

Finally, please add a rule for your example in the repository root
`Makefile` in the section "Examples-related rules", following the pattern
for other examples. This will ensure that your example is built every
time the compiler is modified; if a change in the compiler breaks your example,
the authors will be notified and find a solution.
