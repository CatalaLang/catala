# Catala test suite

This folder contains Catala source files designed to test the features of the
language.

Expected outputs are stored using the convention
`<name_of_test>.catala_<language>.<name_of_scope>.Interpret` in the corresponding test output folder.

For both workflows: use `CATALA_OPTS="..." make ...` to pass in Catala compiler
options when debugging.

## Workflow for adding new tests

1. Create a new test file in `foo/{good,bad}/bar.catala_<language>` (pick the right directory and
   an informative name for your test)
2. Write your test, and pick a toplevel scope `A` to run.
3. From this directory, create `mkdir -p foo/{good,bad}/output` and the empty expected
   output file `touch foo/{good,bad}/output/bar.catala_<language>.A.Interpret`.
4. From the root of the Catala repository, Run `make -C tests foo/{good,bad}/bar.catala_<language>` to get the output of
   your test (compared with the empty file). The failing test will give you the command to use to reproduce
   the fail from the `tests` directory.
5. When you're happy with the output, launch `CLERK_OPTS=--reset make -C tests foo/{good,bad}/bar.catala_<language>`
   from the root of the Catala repository. This
   will record the content of the output of your test into a file.
6. Check that your test pass with `make -C tests foo/{good,bad}/bar.catala_<language>`
   from the root of the Catala repository.
7. That's it, you've added a new test for the Catala language!

## Workflow for fixing regressions

1. Run `make test_suite` from the root of the Catala repository,
   if a test fails you should see something like
   `[ERROR] Test failed: foo/{good,bad}/output/bar.catala_<language>.A.Interpret]`.
2. Compare the computed and expected output with `make -C tests foo/{good,bad}/bar.catala_<language>`
   from the root of the Catala repository.
3. Debug the compiler and/or the test, running `make -C tests foo/{good,bad}/bar.catala_<language>`
   from the root of the Catala repository periodically to check the output of Catala
   on the test case.
4. When you're finished debugging, record the new test output with
   `CLERK_OPTS=--reset make foo/{good,bad}/bar.catala_<language>`.
5. Re-run `make test_suite` from the root of the Catala repository
   to check that everything passes.
6. That's it, you've fixed the Catala test suite to adapt for changes in the
   language.

If a compiler change causes a lot of regressions (error message formatting changes
for instance), you can mass-reset the expected the outputs with `CLERK_OPTS=--reset make test_suite`.
**Caution**: use at your own risk, regressions should be fixed one by one in
general.
