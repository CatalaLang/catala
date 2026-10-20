## Changes since 1.3.0

One line per change, be concise and explicit. Document only external changes
in behavior visible for the end-users of the tooling.

* [#1150](https://github.com/CatalaLang/catala/pull/1150) Fix id collision
  between scopes and topdefs in dependency graphs

* [#1142](https://github.com/CatalaLang/catala/pull/1142) Traces now report
  the declaration position of local variables and tuples

* [#1137](https://github.com/CatalaLang/catala/pull/1137) Fix a bug in
  the java backend that was missing a duration division operator

* [#1132](https://github.com/CatalaLang/catala/pull/1132) Fix issues
  in the Java generation: generated class names could clash in some
  situations, maven groupId was based on verbatim clerk project's name
  which could be syntactically invalid.

* [#1129](https://github.com/CatalaLang/catala/pull/1129) Auto-completion is now
  [available](https://book.catala-lang.org/en/1-1-1-linux-mac-wsl.html?highlight=complet#setting-up-shell-completion)
  in the CLI.

* [#1118](https://github.com/CatalaLang/catala/pull/1118) Clerk variable
  overrides can now refer to the original value of the variable.

* [#1113](https://github.com/CatalaLang/catala/pull/1113) Add the
  `use_libraries` field to `clerk.toml`, allowing external modules to rely on
  external backend-specific libraries.
