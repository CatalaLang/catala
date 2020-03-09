build: format
	dune build

format:
	dune build @fmt --auto-promote

test: build
	dune exec src/main.exe -- --debug --backend LaTeX --output \
		test/allocations_familiales.tex test/allocations_familiales.lawspec
