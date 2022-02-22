# Clerk

The build system for Catala built on top of [ninja](https://ninja-build.org/).

## Build

* to build: `dune build`
* to run: `dune exec ./clerk.exe`

## Usage

See the manpage -- accessible with `clerk --help`.

## Under the hood

To perform tests, Clerk will first generate a `build.ninja` file with required
rules and build statements retrieved from the given input path(s), before
executing the command `ninja test`.

The handling of the `ninja` structure is done with the module `Ninja_utils`.
