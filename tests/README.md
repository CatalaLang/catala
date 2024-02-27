# Catala test suite

This folder contains Catala source files designed to test the features of the
language.

Tests are declared inside plain Catala files with the following format:

~~~markdown
```catala-test-inline
$ catala ARGS
... output from the catala command ...
```
~~~

In addition to the normal Catala commands, you can write `catala test-scope SCOPE`.
This will behave like `catala Interpret -s SCOPE`, but allows to further
customise the flags applied to it (so that the same test can be used to check
with optimisations enabled or disabled, with the lcalc interpreter, and so on)

## Workflow for adding new tests

1. Create a new test file in `foo/{good,bad}/bar.catala_<language>` (pick the right directory and
   an informative name for your test)
2. Write your test, and pick a toplevel scope `A` to run.
3. Add the following section to your file:
      ~~~markdown
      ```catala-test-inline
      $ catala test-scope A
      ```
      ~~~
4. Run `clerk test tests/foo/{good,bad}/bar.catala_<language> --reset`
   from the root of the Catala repository. This will update the test with the
   actual output of the catala command.
5. Don't forget to `git add` the test file.

## Workflow for fixing regressions

1. Run `clerk test` from the root of the Catala repository, if a test fails
   you should see something like
   `FAILED: foo/{good,bad}/bar.catala_<language>` followed by the diff
   between the expected output and the current output of the command.
2. Debug the compiler and the test and repeat. Run
   `clerk test tests/foo/{good,bad}/bar.catala_<language>` to check the test again.
3. When you're finished debugging, if you are positive that a change in the test
   output is legitimate, record the new outputs with
   `clerk test tests/foo/{good,bad}/bar.catala_<language> --reset`.
4. Re-run `make tests` from the root of the Catala repository to check that
   everything passes.
5. Run `git diff` to double-check your changes to the test outputs are expected.
   If necessary, justify them in your commit message.

If a compiler change causes a lot of regressions (error message formatting changes
for instance), you can mass-reset the expected outputs with
`clerk test --reset`.
`git diff` will then allow to check all the changes at once.
**Caution**: It's your responsability to check all the changes before committing them.

## Tips

* Running a single test-file just to check changes when tweaking either the compiler or the test file itself, but without updating or diffing with the reference can be useful when debugging. The following command outputs the result to `stdout` and can be used from within text editors:

      clerk runtest test-file.catala_en
      # Or, to use the current build artefacts, wrap with `dune exec`:
      dune exec -- clerk runtest test-file.catala_en
