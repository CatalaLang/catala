# Clerk

The build system for Catala built on top of [ninja](https://ninja-build.org/).

## Usage

Use `clerk --help` if you have installed it to get more information about the command line
options available. To get the development version of the help, run `make help_clerk`
after `make build`. The `clerk` binary corresponds to the Catala build system,
responsible for testing among other things.

## Under the hood

To perform tests, Clerk will first generate a `build.ninja` file with required
rules and build statements retrieved from the given input path(s), before
executing the command `ninja test`.

The handling of the `ninja` structure is done with the module `Ninja_utils`.
