##########################################
# Dependencies
##########################################

EXECUTABLES = man2html virtualenv python3
K := $(foreach exec,$(EXECUTABLES),\
        $(if $(shell which $(exec)),some string,$(warning [WARNING] No "$(exec)" executable found. \
				Please install this executable for everything to work smoothly)))

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

init-submodules:
	git submodule update --init

install-dependencies: install-dependencies-ocaml init-submodules


##########################################
# Catala compiler rules
##########################################

format:
	dune build @fmt --auto-promote | true

build:
	$(MAKE) -C src/catala/parsing parser_errors.ml
	$(MAKE) format
	dune build

doc: build
	dune build @doc

install: build
	dune build @install

##########################################
# Pygments syntax highilghting rules
##########################################

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

##########################################
# Examples-related rules
##########################################

EXAMPLES_DIR=examples
ALLOCATIONS_FAMILIALES_DIR=$(EXAMPLES_DIR)/allocations_familiales
US_TAX_CODE_DIR=$(EXAMPLES_DIR)/us_tax_code
TUTORIAL_DIR=$(EXAMPLES_DIR)/tutorial

allocations_familiales: pygments build
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) $@.pdf
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) $@.html

us_tax_code: pygments build
	$(MAKE) -C $(US_TAX_CODE_DIR) $@.pdf
	$(MAKE) -C $(US_TAX_CODE_DIR) $@.html

tutorial_en: pygments build
	$(MAKE) -C $(TUTORIAL_DIR) $@.pdf
	$(MAKE) -C $(TUTORIAL_DIR) $@.html

all_examples: allocations_familiales us_tax_code tutorial_en

##########################################
# Website assets
##########################################

grammar.html: src/catala/parsing/parser.mly
	obelisk html -o $@ $<

GENERATE_MAN_PAGE_ARGS=\
	--help=groff | man2html | sed -e '1,8d' \
	| tac | sed "1,18d" | tac

catala.html: src/catala/cli.ml
	dune exec src/catala.exe -- --help=groff | man2html | sed -e '1,8d' \
	| tac | sed "1,20d" | tac > $@

legifrance_catala.html: src/legifrance_catala/main.ml
	dune exec src/legifrance_catala.exe -- --help=groff | man2html | sed -e '1,8d' \
	| tac | sed "1,18d" | tac > $@ > $@

website-assets: doc all_examples grammar.html catala.html legifrance_catala.html

##########################################
# Misceallenous
##########################################

all: install-dependencies install all_examples website-assets

clean:
	dune clean
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) clean
	$(MAKE) -C $(ENGLISH_DUMMY_DIR) clean

inspect:
	gitinspector -f ml,mli,mly,iro,tex,catala,md,ir --grading

##########################################
# Special targets
##########################################
.PHONY: inspect clean all all_examples english allocations_familiales pygments \
	install build format install-dependencies install-dependencies-ocaml \
	catala.html legifrance_catala.html
