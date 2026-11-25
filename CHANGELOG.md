## Changes since 1.3.2

One line per change, be concise and explicit. Document only external changes
in behavior visible for the end-users of the tooling.

* [#1183](https://github.com/CatalaLang/catala/pull/1183) Fixes a wrong
  "type is private" error when the output type of a public scope `Foo`
  of module `Foo` is written `Foo` in another module.
* [#1177](https://github.com/CatalaLang/catala/pull/1177) Fixes a
  "code too large" error in Java that occurs when trying to compile a
  file that has too many tests.
