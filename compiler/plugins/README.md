# Catala compiler plugins

You want to add a customized backend for the Catala compiler but don't
want to modify its source code? Thanks to dynamic linking, it is possible
to do so. The advantage of creating a customized backend is the possibility
to craft a generated target code that perfectly matches the naming conventions,
module structure or coding style of your application.

See the [online documentation](https://catala-lang.org/ocaml_docs/catala/plugins.html)
for more details on how to create them, or look at the existing plugins
in this directory for inspiration.
