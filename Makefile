# Catala compiler rules

install-dependencies:
	opam install \
		ANSITerminal \
		sedlex \
		menhir \
		menhirLib \
		dune \
		cmdliner \
		tls  cohttp lwt cohttp-lwt-unix yojson\
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

EXAMPLES_DIR=examples
ALLOCATIONS_FAMILIALES_DIR=$(EXAMPLES_DIR)/allocations_familiales
ENGLISH_DUMMY_DIR=$(EXAMPLES_DIR)/dummy_english

allocations_familiales: $(PYGMENTIZE) build
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) allocations_familiales.pdf

english: $(PYGMENTIZE) build
	$(MAKE) -C $(ENGLISH_DUMMY_DIR) english.pdf

all_examples: allocations_familiales english

# Misceallenous

all: install-dependencies install all_examples

clean:
	dune clean
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) clean
	$(MAKE) -C $(ENGLISH_DUMMY_DIR) clean

inspect:
	gitinspector -f ml,mli,mly,iro,tex,catala,md,ir --grading

# Special targets

.PHONY: inspect clean all all_examples english allocations_familiales pygments \
	install build format install-dependencies
