build: format
	dune build

install-dependencies:
	opam install \
		ANSITerminal \
		sedlex \
		menhir \
		menhirLib \
		dune \
		cmdliner \
		re
	git submodule update --init

install:
	dune build @install

format:
	dune build @fmt --auto-promote | true

ALLOCATIONS_FAMILIALES_DIR=${CURDIR}/examples/allocations_familiales

PYGMENTS_DIR=${CURDIR}/syntax_highlighting/pygments

PYGMENTIZE=$(PYGMENTS_DIR)/pygments/env/bin/pygmentize

ifdef $(PVC)
  PVC_OPTION=-pvc
else
  PVC_OPTION=
endif

LATEXMK=latexmk $(PVC_OPTION) -g -pdf -halt-on-error -shell-escape


%.tex: %.catala
	dune exec src/main.exe --\
	  --backend LaTeX \
		--debug \
		--wrap_latex \
		--pygmentize=$(PYGMENTIZE)\
	  --output $@ \
		$^

%.pdf: %.tex $(PYGMENTIZE)
	cd $(@D) && $(LATEXMK) $(%F)


$(PYGMENTIZE): $(PYGMENTS_DIR)/set_up_pygments.sh $(PYGMENTS_DIR)/catala.py
	chmod +x $<
	$<
	rm -rf  $(ALLOCATIONS_FAMILIALES_DIR)/_minted-allocations_familiales



allocations_familiales: $(ALLOCATIONS_FAMILIALES_DIR)/allocations_familiales.pdf

inspect:
	gitinspector -f ml,mli,mly,iro,tex,catala,md,ir --grading
