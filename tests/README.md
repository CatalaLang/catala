# Catala test suite

This folder contains Catala source files designed to test the features of the
language.

Tests are declared inside plain Catala files with the following format:

~~~markdown
```catala-test {id="TEST-IDENT"}
catala ARGS
```
~~~

The output of running catala with the given `ARGS` for a `foo.catala_en` file is
expected to be in an `output/foo.catala_en.TEST-IDENT` file (use
`CLERK_OPTS=--reset` to (re-)generate it).

You can use `CATALA_OPTS="..." make ...` to pass in Catala compiler options when
debugging.

## Workflow for adding new tests

1. Create a new test file in `foo/{good,bad}/bar.catala_<language>` (pick the right directory and
   an informative name for your test)
2. Write your test, and pick a toplevel scope `A` to run.
3. Add the following section to your file:
      ~~~markdown
      ```catala-test {id="A.Interpret"}
      catala Interpret -s A
      ```
      ~~~
4. Run `CLERK_OPTS=--reset make tests/foo/{good,bad}/bar.catala_<language>`
   from the root of the Catala repository. This
   will record the content of the output of your test into the target file.
5. Check that the expected output is present in `tests/foo/{good,bad}/output/bar.catala_<language>.A.Interpret`
6. Don't forget to `git add` both the test file and the output file.

## Workflow for fixing regressions

1. Run `make test_suite` from the root of the Catala repository,
   if a test fails you should see something like
   `[ERROR] Test failed: foo/{good,bad}/output/bar.catala_<language>.A.Interpret]`.
2. Compare the computed and expected output with `make tests/foo/{good,bad}/bar.catala_<language>`
   from the root of the Catala repository. Debug the compiler and the test and repeat.
3. When you're finished debugging, if you are positive that a change in the test
   output is legitimate, record the new outputs with `CLERK_OPTS=--reset make
   tests/foo/{good,bad}/bar.catala_<language>`.
4. Re-run `make test_suite` from the root of the Catala repository to check that
   everything passes.
5. Run `git diff` to double-check your changes to the test outputs are expected.
   If necessary, justify them in your commit message.

If a compiler change causes a lot of regressions (error message formatting changes
for instance), you can mass-reset the expected the outputs with `CLERK_OPTS=--reset make test_suite`.
**Caution**: It's your responsability to check all the changes before committing them.
