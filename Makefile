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
	rm -rf  $(ALLOCATIONS_FAMILIALES_DIR)/_minted-allocations_familiales

ifdef $(PVC)
	PVC_OPTION=-pvc
else
	PVC_OPTION=
endif

LATEXMK=latexmk $(PVC_OPTION) -g -pdf -halt-on-error -shell-escape

ALLOCATIONS_FAMILIALES_DIR=${CURDIR}/examples/allocations_familiales
ALLOCATIONS_FAMILIALES_SRC = $(ALLOCATIONS_FAMILIALES_DIR)/allocations_familiales.catala

%.d: %.catala
	touch $@

%.tex: %.catala %.d
	dune exec src/main.exe --\
		--backend LaTeX \
		--debug \
		--wrap_latex \
		--pygmentize=$(PYGMENTIZE)\
		--output $@ \
		$<

%.pdf: %.tex $(PYGMENTIZE)
	cd $(@D) && $(LATEXMK) $(%F)

allocations_familiales: $(ALLOCATIONS_FAMILIALES_DIR)/allocations_familiales.pdf

include $(wildcard $(ALLOCATIONS_FAMILIALES_SRC:.catala=.d))

inspect:
	gitinspector -f ml,mli,mly,iro,tex,catala,md,ir --grading
