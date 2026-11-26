# Contributing to Catala

The project is open to external contributions, in the spirit of open source.
If you want to open a pull request, please follow the instructions below.

Before engaging in a significant contribution, it is recommended to reach out
to the maintainers. Ask questions to the Catala team by:
- [opening an issue](https://github.com/CatalaLang/catala/issues/new) on this repository
- joining the [Zulip chat](https://zulip.catala-lang.org/)

If you want to contribute to the project on a longer-term basis, or if you have
specific competences as a socio-fiscal lawyer or a programming language specialist,
please [contact the authors](mailto:contact@catala-lang.org).
The Catala team meets over visioconference once every week.

Please note that the copyright of this code is owned by Inria;
by contributing, you disclaim all copyright interests in favor of Inria.
Both the code for the compiler and the examples in this repository are
distributed under the Apache2 license.

## Writing Catala code

Before writing Catala code, please read the [Catala book](https://book.catala-lang.org). You can run the
programs of the tutorial yourself by following the instruction in the [README of
the `examples` repository](https://github.com/CatalaLang/catala-examples/blob/master/README.md).
Then, it is suggested that you create a new example directory according to
the instructions of this README.

## Working on the compiler

The Catala compiler is a standard OCaml project that builds using dune.
You can look at the
[online OCaml documentation](https://assets.catala-lang.org/api-doc/catala/) for the
different modules' interfaces as well as high-level architecture documentation.

Note that `catala` and `clerk` can be extended using a [plugin
system](https://assets.catala-lang.org/api-doc/catala/plugins.html): some
extensions may be very useful but have a less generic scope than the core
project. In this case, you may be encouraged to write them as an independent
plugin that lives in a separate repository, that we will be glad to link to.

### Coding guidelines

- Please ensure to submit commits formatted using the included `ocamlformat`
  configuration. The `make build` target ensures that.

  In case the formatting rules or ocamlformat version changed remotely, you can
  use [this
  script](https://gist.github.com/AltGr/2891a61f721c8fd85b1da71e10c691b6) to
  reformat your branch patch by patch before rebasing.

- Please avoid tautological or overly verbose comments.

### On the use of “AI”

We accept AI-assisted contributions, on the following conditions:
- the contributor has a good understanding of all the changes
- they write the pull-request description themselves
- the code changes are limited to the scope of the announced purpose

To ensure the continued quality of the code of the project, maintainers review
every pull request with care. This implies some effort that unfortunately cannot
scale if the contributors cannot make the effort of merely describing the work.

Regardless of the tools they used to write it, contributors are accountable for
the code they submit ; its quality and merits will be evaluated with the same
standards.

### Pull Requests Policy

Pull requests must be approved by, at least, one knowledgable
contributor before merging.

Pull-request branches should not contain merges, and be rebased upon master
whenever possible.

Unless there exists legitimate reasons, every commit of the pull
request should compile, and the final commit must successfully pass the
CI checks.

All requested changes should ideally be included in the PR. However,
if the PR is merged while there are still open discussions or if there
are late remarks, it should be addressed as soon as possible in a
follow-up PR.

As much as possible, offline interactions between the author and
reviewer(s) leading to a discussion resolution should result in a
quick summary that documents the decision.

Whenever major changes are requested, both PR's author and reviewer(s)
may reach an agreement to delay the resolution (e.g., in a future PR)
in which case it must be documented as an issue in order to properly
track it.

### Contribution example: internationalization of the Catala syntax

The Catala language should be adapted to any legislative text that follows a
general-to-specifics statutes order. Therefore, there exists multiple versions
of the Catala surface syntax, adapted to the language of the legislative text.

Currently, Catala supports English, French and Polish legislative text via the
`--language=en`, `--language=fr` or `--language=pl` options.

To add support for a new language:

- the basic syntax localisation is defined in
  `compiler/surface/lexer_xx.cppo.ml` where `xx` is the language code (`en`,
  `fr`...)
- copy the files from another language, e.g.
  [english](compiler/surface/lexer_en.cppo.ml), then replace the strings with your
  translations. Be careful with the following:

  - The file must be encoded in latin-1
  - For a given token `FOO`, define `MS_FOO` to be the string version of the
    keyword. Due to the encoding, use `\xNN` [escape
    sequences](https://ocaml.org/manual/lex.html#escape-sequence) for utf8
    characters.
  - If the string contains spaces or non-latin1 characters, you need to define
    `MR_FOO` as well with a regular expression in [sedlex
    format](https://github.com/ocaml-community/sedlex#lexer-specifications).
    Replace spaces with `", space_plus, "`, and unicode characters with `", 0xNNNN, "` where `NNNN` is the hexadecimal unicode codepoint.

  **Hint:** You may get syntax errors with unhelpful locations because of
  `sedlex`. In that case the command `ocamlc _build/default/compiler/surface/lexer_xx.ml` may point you to the source of the
  error.

- add your translation to the compilation rules:
  - in `compiler/surface/dune`, copying another `parser_xx.cppo.ml` rule
  - in the `extensions` list in `compiler/driver.ml`
  - add a corresponding variant to `compiler/utils/cli.ml` `backend_lang`, try
    to run `make build` and follow all type errors and `match non exhaustive`
    warnings to be sure it is well handled everywhere.
- you may want to add syntax highlighting support, see `syntax_highlighting/`
  and the rules in `Makefile`
- add examples and documentation!

Feel free to open a pull request for discussion even if you couldn't go through
all these steps, the `lexer_xx.cppo.ml` file is the important part.

### Contribution example: writing custom backends as plugins

Catala has support for dynamically-loaded plugins to use as alternative
backends. See `compiler/plugins` for examples, and [the
documentation](https://catala-lang.org/ocaml_docs/catala/plugins.html) for more
detail.
