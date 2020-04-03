build: format
	dune build

format:
	dune build @fmt --auto-promote | true

test: build
	dune exec src/main.exe -- --debug --backend LaTeX --output \
		test/allocations_familiales_new.tex test/allocations_familiales_new.lawspec
