# Catala compiler rules

install-dependencies-ocaml:
	opam install \
		ocamlformat \
		ANSITerminal \
		sedlex \
		menhir \
		menhirLib \
		dune dune-build-info \
		cmdliner obelisk \
		tls  cohttp lwt cohttp-lwt-unix yojson\
		re reason

install-dependencies: install-dependencies-ocaml
	git submodule update --init

format:
	dune build @fmt --auto-promote | true

build:
	$(MAKE) -C src/catala/parsing parser_errors.ml
	dune build
	$(MAKE) format

install: build
	dune build @install

# Pygments syntax highilghting rules

SYNTAX_HIGHLIGHTING_FR=${CURDIR}/syntax_highlighting/fr
PYGMENTS_DIR_FR=$(SYNTAX_HIGHLIGHTING_FR)/pygments
PYGMENTIZE_FR=$(PYGMENTS_DIR_FR)/pygments/env/bin/pygmentize
SYNTAX_HIGHLIGHTING_EN=${CURDIR}/syntax_highlighting/en
PYGMENTS_DIR_EN=$(SYNTAX_HIGHLIGHTING_EN)/pygments
PYGMENTIZE_EN=$(PYGMENTS_DIR_EN)/pygments/env/bin/pygmentize

$(PYGMENTIZE_FR): $(SYNTAX_HIGHLIGHTING_FR)/set_up_pygments.sh $(PYGMENTS_DIR_FR)/catala_fr.py
	chmod +x $<
	$<

$(PYGMENTIZE_EN): $(SYNTAX_HIGHLIGHTING_EN)/set_up_pygments.sh $(PYGMENTS_DIR_EN)/catala_en.py
	chmod +x $<
	$<

pygments: $(PYGMENTIZE_FR) $(PYGMENTIZE_EN)

atom_fr: ${CURDIR}/syntax_highlighting/fr/setup_atom.sh
	chmod +x $<
	$<

atom_en: ${CURDIR}/syntax_highlighting/en/setup_atom.sh
	chmod +x $<
	$<

atom: atom_fr atom_en

# Examples-related rule

EXAMPLES_DIR=examples
ALLOCATIONS_FAMILIALES_DIR=$(EXAMPLES_DIR)/allocations_familiales
ENGLISH_DUMMY_DIR=$(EXAMPLES_DIR)/dummy_english

allocations_familiales: $(PYGMENTIZE_FR) build
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) allocations_familiales.pdf

allocations_familiales_expired: build
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) allocations_familiales.expired

english: $(PYGMENTIZE_EN) build
	$(MAKE) -C $(ENGLISH_DUMMY_DIR) english.pdf

all_examples: allocations_familiales english

# Misceallenous

all: install-dependencies install all_examples

grammar.html: src/catala/parsing/parser.mly
	obelisk html -o $@ $<

catala.html: src/catala/cli.ml
	dune exec src/catala.exe -- --help=groff | man2html | sed -e '1,8d' > $@

legifrance_catala.html: src/legifrance_catala/main.ml
	dune exec src/legifrance_catala.exe -- --help=groff | man2html | sed -e '1,8d'  > $@

clean:
	dune clean
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) clean
	$(MAKE) -C $(ENGLISH_DUMMY_DIR) clean

inspect:
	gitinspector -f ml,mli,mly,iro,tex,catala,md,ir --grading

# Special targets

.PHONY: inspect clean all all_examples english allocations_familiales pygments \
	install build format install-dependencies install-dependencies-ocaml
