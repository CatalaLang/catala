help : Makefile
	@sed -n 's/^#> //p' $<

ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

##########################################
# Dependencies
##########################################

EXECUTABLES = man2html virtualenv python3 colordiff
K := $(foreach exec,$(EXECUTABLES),\
        $(if $(shell which $(exec)),some string,$(warning [WARNING] No "$(exec)" executable found. \
				Please install this executable for everything to work smoothly)))

# The Zarith dependency is fixed because of https://github.com/janestreet/zarith_stubs_js/pull/8
dependencies-ocaml:
	opam install \
		ocamlformat ANSITerminal sedlex	menhir menhirLib dune cmdliner obelisk \
		re obelisk unionfind bindlib zarith.1.11 zarith_stubs_js.v0.14.0 ocamlgraph \
		js_of_ocaml-compiler js_of_ocaml js_of_ocaml-ppx calendar camomile \
		visitors benchmark

init-submodules:
	git submodule update --init

#> dependencies				: Install the Catala OCaml and Git dependencies
dependencies: dependencies-ocaml init-submodules


##########################################
# Catala compiler rules
##########################################

format:
	dune build @fmt --auto-promote 2> /dev/null | true

#> build					: Builds the Catala compiler
build:
	dune build @update-parser-messages --auto-promote | true
	@$(MAKE) --no-print-directory format
	dune build src/catala/catala.exe

#> js_build				: Builds the Web-compatible JS version of the Catala compiler
js_build:
	dune build src/catala/catala_web.bc.js --profile release

#> doc					: Generates the HTML OCaml documentation
doc:
	dune build @doc
	ln -sf $(PWD)/_build/default/_doc/_html/index.html doc/odoc.html

install:
	dune build @install

##########################################
# Syntax highlighting rules
##########################################

SYNTAX_HIGHLIGHTING_FR=${CURDIR}/syntax_highlighting/fr
SYNTAX_HIGHLIGHTING_EN=${CURDIR}/syntax_highlighting/en
SYNTAX_HIGHLIGHTING_PL=${CURDIR}/syntax_highlighting/pl

pygmentize_fr: $(SYNTAX_HIGHLIGHTING_FR)/set_up_pygments.sh
	chmod +x $<
	sudo $<

pygmentize_en: $(SYNTAX_HIGHLIGHTING_EN)/set_up_pygments.sh
	chmod +x $<
	sudo $<

pygmentize_pl: $(SYNTAX_HIGHLIGHTING_PL)/set_up_pygments.sh
	chmod +x $<
	sudo $<

#> pygments				: Extends your pygmentize executable with Catala lexers
pygments: pygmentize_fr pygmentize_en pygmentize_pl

atom_fr: ${CURDIR}/syntax_highlighting/fr/setup_atom.sh
	chmod +x $<
	$<

atom_en: ${CURDIR}/syntax_highlighting/en/setup_atom.sh
	chmod +x $<
	$<

atom_pl: ${CURDIR}/syntax_highlighting/pl/setup_atom.sh
	chmod +x $<
	$<

atom_nv: ${CURDIR}/syntax_highlighting/nv/setup_atom.sh
	chmod +x $<
	$<

#> atom					: Installs Catala syntax highlighting for Atom
atom: atom_fr atom_en atom_nv

vscode_fr: ${CURDIR}/syntax_highlighting/fr/setup_vscode.sh
	chmod +x $<
	$<

vscode_en: ${CURDIR}/syntax_highlighting/en/setup_vscode.sh
	chmod +x $<
	$<

vscode_pl: ${CURDIR}/syntax_highlighting/pl/setup_vscode.sh
	chmod +x $<
	$<

vscode_nv: ${CURDIR}/syntax_highlighting/nv/setup_vscode.sh
	chmod +x $<
	$<

#> vscode					: Installs Catala syntax highlighting for VSCode
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
POLISH_TAXES_DIR=$(EXAMPLES_DIR)/polish_taxes


literate_allocations_familiales: build
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) allocations_familiales.tex
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) allocations_familiales.html

literate_code_general_impots: build
	$(MAKE) -C $(CODE_GENERAL_IMPOTS_DIR) code_general_impots.tex
	$(MAKE) -C $(CODE_GENERAL_IMPOTS_DIR) code_general_impots.html

literate_us_tax_code: build
	$(MAKE) -C $(US_TAX_CODE_DIR) us_tax_code.tex
	$(MAKE) -C $(US_TAX_CODE_DIR) us_tax_code.html

literate_tutorial_en: build
	$(MAKE) -C $(TUTORIAL_EN_DIR) tutorial_en.tex
	$(MAKE) -C $(TUTORIAL_EN_DIR) tutorial_en.html

literate_tutoriel_fr: build
	$(MAKE) -C $(TUTORIEL_FR_DIR) tutoriel_fr.tex
	$(MAKE) -C $(TUTORIEL_FR_DIR) tutoriel_fr.html

literate_polish_taxes: build
	$(MAKE) -C $(POLISH_TAXES_DIR) polish_taxes.tex
	$(MAKE) -C $(POLISH_TAXES_DIR) polish_taxes.html

#> literate_examples			: Builds the .tex and .html versions of the examples code. Needs pygments to be installed and patched with Catala.
literate_examples: literate_allocations_familiales literate_code_general_impots \
	literate_us_tax_code literate_tutorial_en literate_tutoriel_fr literate_polish_taxes

##########################################
# Execute test suite
##########################################

.FORCE:

test_suite: .FORCE
	@$(MAKE) --no-print-directory -C tests pass_tests

test_examples: .FORCE
	@$(MAKE) --no-print-directory -C examples tests

#> tests					: Run interpreter tests
tests: test_suite test_examples

#> tests_ml				: Run OCaml unit tests for the Catala-generated code
tests_ml: run_french_law_library_tests

##########################################
# French law library
##########################################

FRENCH_LAW_LIB_DIR=src/french_law

$(FRENCH_LAW_LIB_DIR)/law_source/allocations_familiales.ml: .FORCE
	CATALA_OPTS="-O -t" $(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) allocations_familiales.ml
	cp -f $(ALLOCATIONS_FAMILIALES_DIR)/allocations_familiales.ml \
		$(FRENCH_LAW_LIB_DIR)/law_source

$(FRENCH_LAW_LIB_DIR)/law_source/unit_tests/tests_allocations_familiales.ml: .FORCE
	CATALA_OPTS="-O -t" $(MAKE) -s -C $(ALLOCATIONS_FAMILIALES_DIR) tests/tests_allocations_familiales.ml
	cp -f $(ALLOCATIONS_FAMILIALES_DIR)/tests/tests_allocations_familiales.ml \
		$(FRENCH_LAW_LIB_DIR)/law_source/unit_tests/

#> generate_french_law_library		: Generates the French law library OCaml sources from Catala
generate_french_law_library:\
	$(FRENCH_LAW_LIB_DIR)/law_source/allocations_familiales.ml \
	$(FRENCH_LAW_LIB_DIR)/law_source/unit_tests/tests_allocations_familiales.ml
	$(MAKE) format

#> build_french_law_library		: Builds the OCaml French law library
build_french_law_library: generate_french_law_library format
	dune build $(FRENCH_LAW_LIB_DIR)/api.a

run_french_law_library_benchmark: generate_french_law_library
	dune exec --profile release $(FRENCH_LAW_LIB_DIR)/bench.exe

run_french_law_library_tests: generate_french_law_library
	dune exec $(FRENCH_LAW_LIB_DIR)/law_source/unit_tests/run_tests.exe

#> build_french_law_library_js		: Builds the JS version of the OCaml French law library
build_french_law_library_js: generate_french_law_library format
	dune build --profile release $(FRENCH_LAW_LIB_DIR)/api_web.bc.js
	cp -f $(ROOT_DIR)/_build/default/$(FRENCH_LAW_LIB_DIR)/api_web.bc.js $(ROOT_DIR)/french_law_js/french_law.js

##########################################
# Website assets
##########################################

grammar.html: src/catala/surface/parser.mly
	obelisk html -o $@ $<

catala.html: src/catala/utils/cli.ml
	dune exec src/catala/catala.exe -- --help=groff | man2html | sed -e '1,8d' \
	| tac | sed "1,20d" | tac > $@

#> website-assets				: Builds all the assets necessary for the Catala website
website-assets: doc literate_examples grammar.html catala.html js_build build_french_law_library_js

##########################################
# Misceallenous
##########################################

all: dependencies build doc tests generate_french_law_library build_french_law_library build_french_law_library_js \
	tests_ml website-assets

#> clean					: Clean build artifacts
clean:
	dune clean
	$(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) clean
	$(MAKE) -C $(US_TAX_CODE_DIR) clean
	$(MAKE) -C $(TUTORIEL_FR_DIR) clean
	$(MAKE) -C $(TUTORIAL_EN_DIR) clean
	$(MAKE) -C $(POLISH_TAXES_DIR) clean
	$(MAKE) -C $(CODE_GENERAL_IMPOTS_DIR) clean

inspect:
	gitinspector -f ml,mli,mly,iro,tex,catala,catala_en,catala_pl,catala_fr,md,fst,mld --grading

##########################################
# Special targets
##########################################
.PHONY: inspect clean all literate_examples english allocations_familiales pygments \
	install build doc format dependencies dependencies-ocaml \
	catala.html help
