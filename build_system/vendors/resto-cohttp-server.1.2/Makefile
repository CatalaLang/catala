
all:
	dune build

.PHONY: test
test:
	dune runtest

doc-html:
	dune build @doc

clean:
	dune clean

fmt:
	ocamlformat --inplace src/*.ml src/*.mli
	ocamlformat --inplace test/*.ml
