# Clerk typecheck integration tests

## `typecheck_test.sh`

Regression test for [PR #987](https://github.com/CatalaLang/catala/pull/987).

**Bug:** `clerk typecheck` on a non-module file (i.e. a file starting with
`> Using ...` rather than `> Module ...`) would generate an empty ninja
`default` target, causing ninja to fail with:

```
ninja: error: expected target name
default
        ^ near here
```

**Root cause:** the fix in #976 made `typecheck_cmd` compute ninja targets
from each file's `module_def`. Non-module files have `module_def = None`, so
the target list was empty.

**Fix:** for non-module files, fall back to the `@src` targets of their
transitive module dependencies (via `linking_dependencies`).

The test sets up a minimal clerk project (`my_module.catala_en` +
`my_test.catala_en`) and verifies that `clerk typecheck my_test.catala_en`
succeeds after `clerk start`.
