## Changes since 1.2.0

One line per change, be concise and explicit. Document only external changes
in behavior visible for the end-users of the tooling.

* [#1082](https://github.com/CatalaLang/catala/pull/1082) New lint warning
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

* [#1072](https://github.com/CatalaLang/catala/pull/1072) Handling of
  dependencies between Clerk targets:
  - Changes in `clerk.toml`:
    * added a `target.dependencies` field
    * removed the `target.include_sources` and `target.include_objects` fields.
      The new `--obj` CLI flag can now be used to install compiled objects to
      the targets directory
    * added a global `include_sources` field (on by default)
    * added a global project `name` field
  - Multiple targets and/or backends can now be specified to `clerk` commands
    `build`, `test` and `run`
  - `clerk` now also allows `--backend all` to compile/test/run all compatible
    backends
  - When building targets defined in `clerk.toml`, `clerk build` now generates
    library definition files in OCaml and Python (more to come)
  - In general, better handling of caching and faster builds (made sure in
    particular that the underlying `ninja` build process is run only once)

* [#1087](https://github.com/CatalaLang/catala/pull/1087/) Escape
  non-ascii characters in Java backend strings as unicode. All Java
  strings are now agnostic of encodings.

* [#1092](https://github.com/CatalaLang/catala/pull/1092) Fixes a bug
  in the `--gen-external` template generator where the generated OCaml
  code did not match the `ExternalType` functor signature: `equal` and
  `compare` were missing the `_pos` parameter, and `from_json` had
  incorrect signature `_pos t` instead of `_pos _s`.

* [#1093](https://github.com/CatalaLang/catala/pull/1093) Improvements
  over java target generation:
  - Added in the generated java target files the proper package and
    import declaration relative to, resp., their own package and
    (transitive) dependencies.
  - Following [#1072](https://github.com/CatalaLang/catala/pull/1072),
    building java targets now also generates a `maven` file
    (`pom.xml`) which can be used to easily compile and generate `jar`
    files.
