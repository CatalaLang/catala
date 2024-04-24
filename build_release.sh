#! /usr/bin/env sh

set -ue

CATALA_VERSION=${CATALA_VERSION:-$(git describe --tags 2>/dev/null || echo dev)}

BIN_TAG=${BIN_TAG:-$(uname -s)-$(uname -m)}

CUSTOM_LINKING_CATALA_Z3="\
(-cclib -static
 -cclib -no-pie
 -noautolink
 -cclib -L/home/ocaml/.opam/z3/lib/stublibs
 -cclib -lz3ml-static
 -cclib -lz3
 -cclib -lstdc++
 -cclib -lthreadsnat
 -cclib -lzarith
 -cclib -lgmp
 -cclib -lcamlstr
 -cclib -lalcotest_stubs
 -cclib -lunix)"

CUSTOM_LINKING_CATALA_NOZ3="(-cclib -static -cclib -no-pie)"

CUSTOM_LINKING_CLERK="(-cclib -static -cclib -no-pie)"

git archive HEAD --prefix catala/ | \
docker run --rm -i registry.gitlab.inria.fr/verifisc/docker-catala:ocaml.4.14-z3static.4.11.2 \
  sh -uexc \
    '{ tar x &&
       cd catala &&
       export CATALA_VERSION='"${CATALA_VERSION}"' &&
       echo "'"${CUSTOM_LINKING_CATALA_Z3}"'" >compiler/custom_linking.sexp &&
       echo "'"${CUSTOM_LINKING_CLERK}"'" >build_system/custom_linking.sexp &&
       opam --cli=2.1 install ./catala.opam --destdir ../release.out/ &&
       mv ../release.out/bin/catala ../release.out/bin/catala-z3 &&
       opam --cli=2.1 remove z3 catala &&
       echo "'"${CUSTOM_LINKING_CATALA_NOZ3}"'" >compiler/custom_linking.sexp &&
       opam --cli=2.1 install ./catala.opam --destdir ../release.out/ &&
       for f in ../release.out/bin/*; do case ${f} in
         *.js) mv ${f} ${f%.js}-'"${CATALA_VERSION}"'.js;;
         *) strip ${f}; mv ${f} ${f}-'"${CATALA_VERSION}"'-'"${BIN_TAG}"';;
       esac; done;
     } >&2 && tar c -hC ../release.out/bin .' |
tar vx "$@"
