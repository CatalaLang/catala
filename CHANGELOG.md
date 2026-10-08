## Changes since 1.3.0

One line per change, be concise and explicit. Document only external changes
in behavior visible for the end-users of the tooling.

* [#1132](https://github.com/CatalaLang/catala/pull/1132) Fix issues
  in the Java generation: generated class names could clash in some
  situations, maven groupId was based on verbatim clerk project's name
  which could be syntactically invalid.
