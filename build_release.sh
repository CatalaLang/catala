#! /usr/bin/env sh

set -ue

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
 -cclib -lANSITerminal_stubs
 -cclib -lunix)"

CUSTOM_LINKING_CATALA_NOZ3="(-cclib -static -cclib -no-pie)"

CUSTOM_LINKING_CLERK="(-cclib -static -cclib -no-pie)"

git archive HEAD --prefix catala/ | \
docker run --rm -i registry.gitlab.inria.fr/lgesbert/catala-ci-images:ocaml.4.14-z3static.4.10.1 \
  sh -uexc \
    '{ tar x &&
       cd catala &&
       echo "'"${CUSTOM_LINKING_CATALA_Z3}"'" >compiler/custom_linking.sexp &&
       echo "'"${CUSTOM_LINKING_CLERK}"'" >build_system/custom_linking.sexp &&
       opam --cli=2.1 install ./ninja_utils.opam ./clerk.opam ./catala.opam --destdir ../release.out/ &&
       mv ../release.out/bin/catala ../release.out/bin/catala-z3 &&
       opam --cli=2.1 remove z3 catala &&
       echo "'"${CUSTOM_LINKING_CATALA_NOZ3}"'" >compiler/custom_linking.sexp &&
       opam --cli=2.1 install ./catala.opam --destdir ../release.out/ &&
       rm -f ../release.out/bin/catala_web_interpreter;
       strip ../release.out/bin/*;
     } >&2 && tar c -hC ../release.out/bin .' |
tar vx
