#
# STAGE 1: setup an opam switch with all dependencies installed
#
# (only depends on the opam files)
FROM ocamlpro/ocaml:4.14-2024-01-14 AS dev-build-context
# Image from https://hub.docker.com/r/ocamlpro/ocaml

RUN mkdir catala
WORKDIR catala

# Get only the opam files at this stage to allow caching
ADD --chown=ocaml:ocaml *.opam ./

# trigger the selection of catala dev tools in opam
ENV OPAMVAR_cataladevmode=1
# TEMPORARILY DISABLED for faster testing
ENV OPAMVAR_catalaz3mode=0

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

# Get the full repo
ADD --chown=ocaml:ocaml . .

# Prepare extra local dependencies
RUN opam exec -- make dependencies-python pygments

# OCaml backtraces may be useful on failure
ENV OCAMLRUNPARAM=b
# Make sure warnings are treated as errors (variable used in Makefile, profile
# defined in ./dune)
ENV DUNE_PROFILE=check

# Check the build
RUN opam exec -- make build js_build

# Install to prefix
RUN opam exec -- make install
