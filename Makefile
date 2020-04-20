# Catala compiler rules

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

format:
		dune build @fmt --auto-promote | true

build: format
	dune build

install: build
	dune build @install

# Pygments syntax highilghting rules

PYGMENTS_DIR=${CURDIR}/syntax_highlighting/pygments

PYGMENTIZE=$(PYGMENTS_DIR)/pygments/env/bin/pygmentize

$(PYGMENTIZE): $(PYGMENTS_DIR)/set_up_pygments.sh $(PYGMENTS_DIR)/catala.py
	chmod +x $<
	$<

pygments: $(PYGMENTIZE)

# Examples-related rule

allocations_familiales: $(PYGMENTIZE) build
	$(MAKE) -C examples/allocations_familiales allocations_familiales.pdf

english: $(PYGMENTIZE) build
	$(MAKE) -C examples/dummy_english english.pdf

all_examples: allocations_familiales english

# Misceallenous

all: install-dependencies install all_examples

clean:
	dune clean
	$(MAKE) -C examples/allocations_familiales clean
	$(MAKE) -C examples/dummy_english clean

inspect:
	gitinspector -f ml,mli,mly,iro,tex,catala,md,ir --grading
