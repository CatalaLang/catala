## Rules for contributing

1. You disclaim copyright interests in favor of Inria (code is distributed under the Apache2 license)
2. You have a good understanding of all the changes you are proposing
3. You are not a robot, and you have written the pull-request description

See [CONTRIBUTING.md](https://github.com/CatalaLang/catala/blob/master/CONTRIBUTING.md) for details.

## Checklist (please remove non-applicable items)

* [ ] Update the CHANGELOG.md (only for significant user-visible changes)

### If this PR adds a feature or has breaking changes

* [ ] Complete or update the documentation for the feature on the Catala book at https://github.com/CatalaLang/catala-book.
  → Corresponding PR: https://github.com/CatalaLang/catala-book/pull/
* [ ] Update the existing Catala code in case of breaking changes at https://github.com/CatalaLang/catala-examples.
  → Corresponding PR: https://github.com/CatalaLang/catala-examples/pull/
* [ ] Update the Catala language server in case of breaking changes at https://github.com/CatalaLang/catala-language-server
  → Corresponding PR: https://github.com/CatalaLang/catala-language-server/pull/

### If this PR contains syntax changes

* [ ] Syntax cheat sheet at `doc/syntax/syntax_*.catala_*` and `doc/syntax/catala_*.typ`.
* [ ] Syntax highlighting plugins in `syntax_highlighting/` (all of them).
* [ ] Syntax constructions in the `tree-sitter` grammar:
  - Base grammar: https://github.com/CatalaLang/tree-sitter-catala/blob/master/grammar.js
    → Corresponding PR: https://github.com/CatalaLang/tree-sitter-catala/pull/
  - Formatting spec: https://github.com/CatalaLang/catala-format/blob/master/catala.scm
    → Corresponding PR: https://github.com/CatalaLang/catala-format/pull/
