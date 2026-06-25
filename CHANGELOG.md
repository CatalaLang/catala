## Changes since 1.2.0

One line per change, be concise and explicit. Document only external changes
in behavior visible for the end-users of the tooling.

* [#1058](https://github.com/CatalaLang/catala/pull/1058) Fixes a bug
  in the JSON output format of enumerations yielding errors such as:
  `Invalid_argument("Json_encoding.construct: consequence of non
  exhaustive Json_encoding.string_enum` and
  `Invalid_argument("Json_encoding.construct: consequence of bad
  union")`

* [#1069](https://github.com/CatalaLang/catala/pull/1069) Revamp of
  the `--trace` mechanism:
  - Added support in the `Java` backend;
  - Added `clerk run --trace ...` options.
