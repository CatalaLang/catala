

## Checklist

### If this PR adds a feature or has breaking changes

* [ ] Complete or update the documentation for the feature on the Catala book at https://github.com/CatalaLang/catala-book.
  * [ ] The corresponding PR is:
* [ ] Update the existing Catala code in case of breaking changes at https://github.com/CatalaLang/catala-examples.
  * [ ] The corresponding PR is:
* [ ] Update the Catala language server in case of breaking changes at https://github.com/CatalaLang/catala-language-server
  * [ ] The corresponding PR is:

### If this PR contains syntax changes

I confirm that have have checked and updated each of the following items if this PR impacts them:

* [ ] Syntax cheat sheet at `doc/syntax/syntax_*.catala_*` and `doc/syntax/catala_*.typ`.
* [ ] Syntax highlighting plugins in `syntax_highlighting/` (all of them).
* [ ] Syntax constructions in the `tree-sitter` grammar:
  - Base grammar: https://github.com/CatalaLang/tree-sitter-catala/blob/master/grammar.js
    - [ ] The corresponding PR is:
  - Formatting spec: https://github.com/CatalaLang/catala-format/blob/master/catala.scm
    - [ ] The corresponding PR is:
