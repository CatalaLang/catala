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
		js_of_ocaml-compiler js_of_ocaml js_of_ocaml-ppx calendar camomile \
		visitors benchmark

init-submodules:
	git submodule update --init

dependencies: dependencies-ocaml init-submodules


##########################################
# Catala compiler rules
##########################################

format:
	dune build @fmt --auto-promote 2> /dev/null | true 

build:
	@$(MAKE) --no-print-directory -C src/catala/surface parser_errors.ml
	@$(MAKE) --no-print-directory format
	dune build src/catala/catala.exe

js_build:
	dune build src/catala/catala_web.bc.js --profile release

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

tests_ml: run_french_law_library_tests

##########################################
# French law library
##########################################

FRENCH_LAW_LIB_DIR=src/french_law

$(FRENCH_LAW_LIB_DIR)/law_source/allocations_familiales.ml:
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) allocations_familiales.ml
	cp -f $(ALLOCATIONS_FAMILIALES_DIR)/allocations_familiales.ml \
		$(FRENCH_LAW_LIB_DIR)/law_source 

french_law_library:\
	$(FRENCH_LAW_LIB_DIR)/law_source/allocations_familiales.ml

run_french_law_library_benchmark: french_law_library
	dune exec $(FRENCH_LAW_LIB_DIR)/bench.exe

$(FRENCH_LAW_LIB_DIR)/law_source/unit_tests/tests_allocations_familiales.ml:
	@$(MAKE) --no-print-directory -s -C $(ALLOCATIONS_FAMILIALES_DIR) tests/tests_allocations_familiales.ml
	@cp -f $(ALLOCATIONS_FAMILIALES_DIR)/tests/tests_allocations_familiales.ml \
		$(FRENCH_LAW_LIB_DIR)/law_source/unit_tests/

french_law_library_tests: \
	$(FRENCH_LAW_LIB_DIR)/law_source/unit_tests/tests_allocations_familiales.ml

run_french_law_library_tests: french_law_library_tests
	@dune exec $(FRENCH_LAW_LIB_DIR)/law_source/unit_tests/run_tests.exe

build_french_law_library: format
	dune build $(FRENCH_LAW_LIB_DIR)

build_french_law_library_js: french_law_library format
	dune build --profile release $(FRENCH_LAW_LIB_DIR)/api_web.bc.js
	ln -sf $(PWD)/_build/default/$(FRENCH_LAW_LIB_DIR)/api_web.bc.js javascript/french_law.js

##########################################
# Website assets
##########################################

grammar.html: src/catala/surface/parser.mly
	obelisk html -o $@ $<

catala.html: src/catala/utils/cli.ml
	dune exec src/catala/catala.exe -- --help=groff | man2html | sed -e '1,8d' \
	| tac | sed "1,20d" | tac > $@

website-assets: doc literate_examples grammar.html catala.html js_build build_french_law_library_js

##########################################
# Misceallenous
##########################################

all: dependencies build doc tests literate_examples website-assets

clean:
	dune clean
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) clean
	$(MAKE) -C $(US_TAX_CODE_DIR) clean
	$(MAKE) -C $(TUTORIEL_FR_DIR) clean
	$(MAKE) -C $(TUTORIAL_EN_DIR) clean
	$(MAKE) -C $(CODE_GENERAL_IMPOTS_DIR) clean

inspect:
	gitinspector -f ml,mli,mly,iro,tex,catala,catala_en,catala_fr,md,fst,mld --grading

##########################################
# Special targets
##########################################
.PHONY: inspect clean all literate_examples english allocations_familiales pygments \
	install build doc format dependencies dependencies-ocaml \
	catala.html 
