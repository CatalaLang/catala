# Catala: A Programming Language for the Law

Welcome to the ICFP artefact for the Catala compiler. This artefact is a modified
version of the GitHub repository at https://github.com/CatalaLang/catala.
The regular readme was renamed to `README.WEB.md` to be replaced by this one,
which has been written specifically for the ICFP artefact. Please make sure you
`cd` into the `catala` directory of the VM, which is assume to be the base
directory of all the commands listed below.

## Building the Catala compiler

The compiler should already be built and available as `catala.exe`. It is
not available in the `$PATH`, but is placed You can
also re-trigger the build with `make build`.

## Writing your first Catala program

Open a file `foo.catala_en` and type in:

~~~
## My Catala program

```catala
declaration scope Foo:
  context bar content money
  context baz content decimal
  context foobar content money
  context select content boolean

scope Foo:
  definition foobar equals bar *$ baz

  exception
  definition foobar under condition select consequence equals
    bar *$ (baz *. 2.0)

declaration scope FooTest:
   context foo scope Foo
   context foobar content money

scope FooTest:
  definition foo.bar equals $100
  definition foo.baz equals 0.2
  definition foo.select equals true
  definition foobar equals foo.foobar
```
~~~

You can now run this program by running the interpreter:

    ./catala.exe --language=en --scope=FooTest Interpret foo.catala_en

Explanation of the options:
* `--language=en` tells the compiler to parse the English version of the Catala keywords.
* `--scope=FooTest` tells the interpeter to run the function corresponding to the scope
  `FooTest`. It is the equivalent of specifying which is the "main" function.

You can now fiddle with this base to write any program that you want. You can
also see the traduction of your source program in the default calculus formalism
by entering

    ./catala.exe --language=en --scope=Foo --dcalc --unstyled Interpret foo.catala_en

## Tutorial and syntax

A tutorial presenting the features of the language is present in `examples/tutorial_en`.
You can run the interpreter on the tutorial by entering

    SCOPE=Test3 make -C examples/tutorial_en tutorial_en.run

To get a sense of Catala's syntax, you can look at the suggestions in the
syntax error messages of the compiler. A syntax cheat sheet is also available
at https://github.com/CatalaLang/catala/blob/master/doc/syntax/syntax.pdf
or in the `doc/syntax/syntax.pdf` file of this repository.

## Examples from the paper

### Section 121 of the US Tax Code

The full code for the example from Appendix A of the paper (available in the
supplementary PDF material) is available at `examples/us_tax_code/section121.catala_en`.
Please note that there are slight differences between the full code of Section
121 presented in Appendix A and the various excerpts shown in sections §2 and
§3 of the paper. Indeed, those excerpts show a somewhat simplified version of
the code whose purpose is to present the features of Catala and not be a
self-contained piece of executable code.

You can run the unit tests in `examples/us_tax_code/tests/test_section_121.catala_en`
with this kind of command:

    SCOPE=Test1 make -C examples/us_tax_code tests/test_section_121.run

### French family benefits

The full code for the example from section §6.3 of the paper is available at
`examples/allocations_familiales/*.catala_fr`. Note that the source code in this
file is written with the French version of the Catala surface language, which is
not meant to be understood by non-francophones (rather, it is intended to be
understood by francophone programmers and lawyers). You can run the unit tests
in `examples/allocations_familiales/tests/tests_allocations_familiales.catala_en`
with this kind of command:

    SCOPE=Test1 make -C examples/allocations_familiales tests/tests_allocations_familiales.run

As claimed in the paper, the Catala source code for the French family benefits
can also be compiled to OCaml. To trigger this build, please run

    make generate_french_law_library -B

The generated code can be inspected at `examples/allocations_familiales/allocations_familiales.ml`,
or in `src/french_law/law_source/allocations_familiales.ml`. You can run the
same unit tests as before for the generated OCaml code by typing

    make tests_ml

You can also benchmark the generated code on random inputs by typing

    make run_french_law_library_benchmark


## User study

The full data for the user study presented in section §6.1 of the paper can
be found at `doc/user_study/catala_case_study_us_tax_code_section_121.xlsx`.

## F* formalization

The F* formalization of the proof presented in section §4.4 of the paper can
be found at `doc/formalization`. Please refer to the README in this directory
for further description and instructions on how to replay the proof.
