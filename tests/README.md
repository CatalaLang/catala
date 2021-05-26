# Catala test suite

This folder contains Catala source files designed to test the features of the
language.

It uses `make pass_tests` to launch tests and compare the test terminal output
with an expected output.

Expected outputs are stored using the convention
`<name_of_test>.catala_<language>.<name_of_scope>.out` in the corresponding test output folder.

For both workflows: use `CATALA_OPTS="..." make ...` to pass in Catala compiler
options when debugging.

## Workflow for adding new tests

1. Create a new test file in `foo/{good,bad}/bar.catala_<language>` (pick the right directory and
   an informative name for your test)
2. Write your test, and pick a toplevel scope `A` to run.
3. From this directory, launch `make foo/{good,bad}/bar.catala_<language>.A.run` to get the output of
   your test.
4. When you're happy with the output, launch `make foo/{good,bad}/bar.catala_<language>.A.in`. This
   will record the content of the output of your test into a file.
5. Check that your test pass with `make foo/{good,bad}/output/bar.catala_<language>.A.out`.
6. That's it, you've added a new test for the Catala language!

## Workflow for fixing regressions

1. Run `make`, if a test fails you should see something like
   `[FAIL foo/{good,bad}/output/bar.catala_<language>.A]`.
2. Compare the computed and expected output with `make foo/{good,bad}/output/bar.catala_<language>.A.out`.
3. Debug the compiler and/or the test, running `make foo/{good,bad}/bar.catala_<language>.A.run`
   periodically to check the output of Catala on the test case.
4. When you're finished debugging, record the new test output with
   `make foo/{good,bad}/bar.catala_<language>.A.in`.
5. Re-run `make` to check that everything passes.
6. That's it, you've fixed the Catala test suite to adapt for changes in the
   language.

If a compiler change causes a lot of regressions (error message formatting changes
for instance), you can mass-reset the expected the outputs with `make reset_tests`.
**Caution**: use at your own risk, regressions should be fixed one by one in
general.
