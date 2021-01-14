default: build

##########################################
# Dependencies
##########################################

EXECUTABLES = man2html virtualenv python3 colordiff
K := $(foreach exec,$(EXECUTABLES),\
        $(if $(shell which $(exec)),some string,$(warning [WARNING] No "$(exec)" executable found. \
				Please install this executable for everything to work smoothly)))

dependencies-ocaml:
	opam install \
		ocamlformat ANSITerminal sedlex	menhir menhirLib dune cmdliner obelisk \
		re obelisk unionfind bindlib zarith zarith_stubs_js ocamlgraph \
		js_of_ocaml-compiler js_of_ocaml js_of_ocaml-ppx calendar camomile

init-submodules:
	git submodule update --init

dependencies: dependencies-ocaml init-submodules


##########################################
# Catala compiler rules
##########################################

format:
	dune build @fmt --auto-promote | true

build:
	@$(MAKE) --no-print-directory -C src/catala/catala_surface parser_errors.ml
	@$(MAKE) --no-print-directory format
	dune build

release_build:
	dune build --profile release

doc: 
	dune build @doc
	ln -sf $(PWD)/_build/default/_doc/_html/index.html doc/odoc.html

install:
	dune build @install

##########################################
# Syntax highlighting rules
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

atom_nv: ${CURDIR}/syntax_highlighting/nv/setup_atom.sh
	chmod +x $<
	$<

atom: atom_fr atom_en atom_nv

vscode_fr: ${CURDIR}/syntax_highlighting/fr/setup_vscode.sh
	chmod +x $<
	$<

vscode_en: ${CURDIR}/syntax_highlighting/en/setup_vscode.sh
	chmod +x $<
	$<

vscode_nv: ${CURDIR}/syntax_highlighting/nv/setup_vscode.sh
	chmod +x $<
	$<

vscode: vscode_fr vscode_en vscode_nv

##########################################
# Examples-related rules
##########################################

EXAMPLES_DIR=examples
ALLOCATIONS_FAMILIALES_DIR=$(EXAMPLES_DIR)/allocations_familiales
CODE_GENERAL_IMPOTS_DIR=$(EXAMPLES_DIR)/code_general_impots
US_TAX_CODE_DIR=$(EXAMPLES_DIR)/us_tax_code
TUTORIAL_EN_DIR=$(EXAMPLES_DIR)/tutorial_en
TUTORIEL_FR_DIR=$(EXAMPLES_DIR)/tutoriel_fr


literate_allocations_familiales: pygments build
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) allocations_familiales.tex
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) allocations_familiales.html

literate_code_general_impots: pygments build
	$(MAKE) -C $(CODE_GENERAL_IMPOTS_DIR) code_general_impots.tex
	$(MAKE) -C $(CODE_GENERAL_IMPOTS_DIR) code_general_impots.html

literate_us_tax_code: pygments build
	$(MAKE) -C $(US_TAX_CODE_DIR) us_tax_code.tex
	$(MAKE) -C $(US_TAX_CODE_DIR) us_tax_code.html

literate_tutorial_en: pygments build
	$(MAKE) -C $(TUTORIAL_EN_DIR) tutorial_en.tex
	$(MAKE) -C $(TUTORIAL_EN_DIR) tutorial_en.html

literate_tutoriel_fr: pygments build
	$(MAKE) -C $(TUTORIEL_FR_DIR) tutoriel_fr.tex
	$(MAKE) -C $(TUTORIEL_FR_DIR) tutoriel_fr.html

literate_examples: literate_allocations_familiales literate_code_general_impots \
	literate_us_tax_code literate_tutorial_en literate_tutoriel_fr

##########################################
# Execute test suite
##########################################

.FORCE:

test_suite: .FORCE
	@$(MAKE) --no-print-directory -C tests pass_tests

test_examples: .FORCE 
	@$(MAKE) --no-print-directory -C examples tests

tests: test_suite test_examples

##########################################
# Website assets
##########################################

grammar.html: src/catala/catala_surface/parser.mly
	obelisk html -o $@ $<

catala.html: src/catala/utils/cli.ml
	dune exec src/catala.exe -- --help=groff | man2html | sed -e '1,8d' \
	| tac | sed "1,20d" | tac > $@

website-assets: doc literate_examples grammar.html catala.html release_build

##########################################
# Misceallenous
##########################################

all: dependencies build doc tests literate_examples website-assets

clean:
	dune clean
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) clean
	$(MAKE) -C $(US_TAX_CODE_DIR) clean
	$(MAKE) -C $(TUTORIAL_DIR) clean
	$(MAKE) -C $(CODE_GENERAL_IMPOTS_DIR) clean

inspect:
	gitinspector -f ml,mli,mly,iro,tex,catala,catala_en,catala_fr,md,fst,mld --grading

##########################################
# Special targets
##########################################
.PHONY: inspect clean all literate_examples english allocations_familiales pygments \
	install build doc format dependencies dependencies-ocaml \
	catala.html 
