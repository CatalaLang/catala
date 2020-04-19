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

build: format
	dune build

install:
	dune build @install

format:
	dune build @fmt --auto-promote | true

PYGMENTS_DIR=${CURDIR}/syntax_highlighting/pygments

PYGMENTIZE=$(PYGMENTS_DIR)/pygments/env/bin/pygmentize

$(PYGMENTIZE): $(PYGMENTS_DIR)/set_up_pygments.sh $(PYGMENTS_DIR)/catala.py
	chmod +x $<
	$<
	rm -rf  $(SRC_DIR)/_minted-allocations_familiales

ifdef $(PVC)
	PVC_OPTION=-pvc
else
	PVC_OPTION=
endif

LATEXMK=latexmk $(PVC_OPTION) -g -pdf -halt-on-error -shell-escape

SRC_DIR=${CURDIR}/examples/allocations_familiales
SRC = $(SRC_DIR)/allocations_familiales.catala


%.tex: %.catala 
	dune exec src/main.exe -- Makefile $<
	dune exec src/main.exe --\
		--debug \
		--wrap_latex \
		--pygmentize=$(PYGMENTIZE)\
		LaTeX \
		$<

%.pdf: %.tex $(PYGMENTIZE)
	cd $(@D) && $(LATEXMK) $(%F)

include $(wildcard $(SRC:.catala=.d))

inspect:
	gitinspector -f ml,mli,mly,iro,tex,catala,md,ir --grading
