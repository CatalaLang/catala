## Changes since 1.2.0

One line per change, be concise and explicit. Document only external changes
in behavior visible for the end-users of the tooling.

* [#591](https://github.com/CatalaLang/catala/issues/591) New lint warning
  for local variables (`let ... in`) that are never used.

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

* [#1075](https://github.com/CatalaLang/catala/pull/1075) Fixes a bug
  where, for large Catala programs, the java generated code is too
  large and would yield "code too large" error. We now split large
  methods into smaller methods whenever necessary.
  Also, removed law headings from positions in Java (same as C).
