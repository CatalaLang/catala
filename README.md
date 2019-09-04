# Verifisc

Verifisc is a low-level intermediate representation designed to be a common target
for languages and DSL describing legal specifications. The goal is to run different
passes of abstract interpretation and symbolic execution on Verifisc, or to translate
it directly to Z3 in order to gain insight on the legislation that the code describes.

## Usage

The project is distributed as a Dune artifact. Use standard dune commands to build
and install the library.

## License

The library is released under the Apache license (version 2).
