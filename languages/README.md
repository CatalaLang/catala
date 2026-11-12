# Adding a new language syntax

Catala is able to handle localized syntaxes. This process is mostly
generic and only requires users to provide the set of lexing tokens
that Catala needs. We rely on the same language grammar for all,
hence, only the keywords and built-ins names can be changed.

These next sections describe the required process necessary to add
support to the Catala and its tooling ecosystem.

## Catala compiler

In the Catala repository:

- Provide a valid JSON syntax file named after your language. You may
  copy over 'languages/english.json' and adapt the existing tokens
  from there. The `code` key must be a two letter identifier (e.g.,
  `"en"`, `"fr"`, `"pl"`, etc.). UTF-8 characters are accepted;

- Run `dune build` twice: once to generate the meta-build files and
  twice to build using the newly generated configurations;

- Commit your changes and install this new version using `opam install .`.

## Catala language server

Next, clone the
[catala-language-server](https://github.com/CatalaLang/catala-language-server/)
repository. Then:

- Run `dune build` twice: once to generate the meta-build files and
  twice to build using the newly generated configurations.

  **Hint** If the new version of Catala with the language is detected,
  you should see several new files appearing in the `git` diff

- Commit your changes, run `make` to build the VScode extension and,
  running VScode in development mode, test that writing a Catala file
  with your new extension, for example `my_test.catala_XX` where `XX`
  is your language code, correctly highlights the keywords and that
  the LSP correctly detects errors.

## `catala-format`

`catala-format` relies on a `tree-sitter` grammar defined in the
[tree-sitter-catala](https://github.com/CatalaLang/tree-sitter-catala)
repository.

The first step is to generate the new tree-sitter grammar for your
language. This is also done automatically:

- Cloning the repository and running `make` should pick-up, generate
  and compile the new `tree-sitter` grammar.

  **Hint** Again, verify that you see the new files appearing in the
  `git` diff

- Commit the changes (along with the new generated files) and push
  them on the repository. If you do not have the permission, please
  reach out to us on the [Zulip](https://zulip.catala-lang.org/). We will do it for you.

Now that the tree-sitter is patched, we can update the
[catala-format](https://github.com/CatalaLang/catala-format)
repository:

- Clone the repository, update the `CATALA-TREE-SITTER-HASH` file with
  the commit hash of the previously pushed version;

- Run `dune build` to generate a new `topiary` configuration file
  that will accept Catala files with the new extensions.

- Commit your changes and install this new version of `catala-format`
  using `opam install .`. Verify that `catala-format` correctly
  formats a well-formed Catala file written with your syntax.

## Upstreaming your new syntax

If you wish to upstream your new syntax in the Catala language, you
will need, for each of the modified repository, to provide a separate
pull request on each of them. The main pull request, made on the main
Catala repository, should have a link to the other PRs.

## Current limitations

### Standard library

Currently, we do not generate translated versions of the standard
libraries. New syntaxes will default to the english one being
available. The underlying mechanism around the standard library being
complex, we currently reject upstreaming localized stdlibs.

### VScode extension

The GUI comes with its own localized messages and snippets which are
not generated. It is, however, possible to add a translation of these
files if desired.
