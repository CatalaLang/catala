# This Dockerfile is only meant for the CI of the development of the
# Catala compiler itself. If you want a Docker image for the base
# of the CI of your Catala project, you should head over to
# https://gitlab.inria.fr/verifisc/docker-catala/container_registry/1606.
#
# STAGE 1: setup an opam switch with all dependencies installed
#
# (only depends on the opam files)
FROM ocamlpro/ocaml:4.14-2025-07-27 AS dev-build-context
# Image from https://hub.docker.com/r/ocamlpro/ocaml

RUN mkdir catala
WORKDIR catala

# Get only the opam files at this stage to allow caching
ADD --chown=ocaml:ocaml *.opam ./

# trigger the selection of catala dev tools in opam
ENV OPAMVAR_cataladevmode=1
ENV OPAMVAR_catalaz3mode=1

# FIXME: openjdk's opam package should handle alpine os
RUN sudo apk add openjdk21 su-exec

# FIXME: pygments is in catala.opam's depexts but the depexts don't handle
# the --with-dev-setup option, hence theye're never installed by the command
# below...
RUN sudo apk add py3-pip py3-pygments groff bash

# Get a switch with all the dependencies installed
# DON'T run 'opam update' here. Instead use a newer parent Docker image
# (update the 'FROM' line above)
RUN opam --cli=2.2 switch create . --deps-only --with-test --with-doc --with-dev-setup && \
    opam clean

#
# STAGE 2: get the whole repo and build
#
FROM dev-build-context

# Prepare extra local dependencies (doing this first allows caching)
ADD --chown=ocaml:ocaml runtimes/python/pyproject.toml runtimes/python/pyproject.toml
ADD --chown=ocaml:ocaml deps/dates-calc/lib_python/pyproject.toml deps/dates-calc/lib_python/pyproject.toml
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
RUN opam exec -- make build

# Install to prefix
RUN opam exec -- make install && opam clean
