#
# STAGE 1: setup an opam switch with all dependencies installed
#
# (only depends on the opam files)
FROM ocamlpro/ocaml:4.14-2024-05-26 AS dev-build-context
# Image from https://hub.docker.com/r/ocamlpro/ocaml

RUN mkdir catala
WORKDIR catala

# Get only the opam files at this stage to allow caching
ADD --chown=ocaml:ocaml *.opam ./

# trigger the selection of catala dev tools in opam
ENV OPAMVAR_cataladevmode=1
ENV OPAMVAR_catalaz3mode=1

# Get a switch with all the dependencies installed
# DON'T run 'opam update' here. Instead use a newer parent Docker image
# (update the 'FROM' line above)
RUN opam --cli=2.1 switch create catala ocaml-system && \
    opam --cli=2.1 install . --with-test --with-doc --depext-only && \
    opam --cli=2.1 install . --with-test --with-doc --deps-only && \
    opam clean
# Note: just `opam switch create . --deps-only --with-test --with-doc && opam clean`
# should be enough once opam 2.2 is released (see opam#5185)

#
# STAGE 2: get the whole repo and build
#
FROM dev-build-context

# Prepare extra local dependencies (doing this first allows caching)
ADD --chown=ocaml:ocaml runtimes/python/pyproject.toml runtimes/python/pyproject.toml
ADD --chown=ocaml:ocaml Makefile .
ADD --chown=ocaml:ocaml syntax_highlighting syntax_highlighting
RUN opam exec -- make dependencies-python pygments

# Get the full repo
ADD --chown=ocaml:ocaml . .

# OCaml backtraces may be useful on failure
ENV OCAMLRUNPARAM=b
# Make sure warnings are treated as errors (variable used in Makefile, profile
# defined in ./dune)
ENV DUNE_PROFILE=check

ARG CATALA_VERSION

# Check the build
RUN opam exec -- make build js_build

# Install to prefix
RUN opam exec -- make install && opam clean
