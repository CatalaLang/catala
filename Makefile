help : Makefile
	@sed -n 's/^#> //p' $<

ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

# Export all variables to sub-make
export

##########################################
# Dependencies
##########################################

EXECUTABLES = man2html virtualenv python3 colordiff node pygmentize nodejs npm ninja
K := $(foreach exec,$(EXECUTABLES),\
        $(if $(shell which $(exec)),some string,$(warning [WARNING] No "$(exec)" executable found. \
				Please install this executable for everything to work smoothly)))

dependencies-ocaml:
	opam install . --deps-only --with-doc --with-test --yes

dependencies-js:
	$(MAKE) -C $(FRENCH_LAW_JS_LIB_DIR) dependencies

init-submodules:
	git submodule update --init

#> dependencies				: Install the Catala OCaml, JS and Git dependencies
dependencies: dependencies-ocaml dependencies-js init-submodules


##########################################
# Catala compiler rules
##########################################

COMPILER_DIR=compiler
BUILD_SYSTEM_DIR=build_system

format:
	dune build @fmt --auto-promote 2> /dev/null | true

#> build_dev				: Builds the Catala compiler, without formatting code
build_dev:
	dune build @update-parser-messages --auto-promote | true
	dune build $(COMPILER_DIR)/catala.exe
	dune build $(BUILD_SYSTEM_DIR)/clerk.exe

#> build					: Builds the Catala compiler
build:
	dune build @update-parser-messages --auto-promote | true
	@$(MAKE) --no-print-directory format
	dune build $(COMPILER_DIR)/catala.exe
	dune build $(BUILD_SYSTEM_DIR)/clerk.exe

#> js_build				: Builds the Web-compatible JS versions of the Catala compiler
js_build:
	dune build $(COMPILER_DIR)/catala.bc.js --profile release
	dune build $(COMPILER_DIR)/catala_web_interpreter.bc.js --profile release

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

#> atom					: Installs Catala syntax highlighting for Atom
atom: atom_fr atom_en atom_pl

vscode_fr: ${CURDIR}/syntax_highlighting/fr/setup_vscode.sh
	chmod +x $<
	$<

vscode_en: ${CURDIR}/syntax_highlighting/en/setup_vscode.sh
	chmod +x $<
	$<

# TODO
# vscode_pl: ${CURDIR}/syntax_highlighting/pl/setup_vscode.sh
# 	chmod +x $<
# 	$<

#> vscode					: Installs Catala syntax highlighting for VSCode
vscode: vscode_fr vscode_en

##########################################
# Literate programming and examples
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
# French law library
##########################################

#-----------------------------------------
# OCaml
#-----------------------------------------

FRENCH_LAW_OCAML_LIB_DIR=french_law/ocaml

$(FRENCH_LAW_OCAML_LIB_DIR)/law_source/allocations_familiales.ml:
	CATALA_OPTS="$(CATALA_OPTS) -O -t" $(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) allocations_familiales.ml
	cp -f $(ALLOCATIONS_FAMILIALES_DIR)/allocations_familiales.ml $@

$(FRENCH_LAW_OCAML_LIB_DIR)/law_source/unit_tests/tests_allocations_familiales.ml:
	CATALA_OPTS="$(CATALA_OPTS) -O -t" $(MAKE) -s -C $(ALLOCATIONS_FAMILIALES_DIR) tests/tests_allocations_familiales.ml
	cp -f $(ALLOCATIONS_FAMILIALES_DIR)/tests/tests_allocations_familiales.ml $@

#> generate_french_law_library_ocaml	: Generates the French law library OCaml sources from Catala
generate_french_law_library_ocaml:\
	$(FRENCH_LAW_OCAML_LIB_DIR)/law_source/allocations_familiales.ml \
	$(FRENCH_LAW_OCAML_LIB_DIR)/law_source/unit_tests/tests_allocations_familiales.ml
	$(MAKE) format

#> build_french_law_library_ocaml		: Builds the OCaml French law library
build_french_law_library_ocaml: generate_french_law_library_ocaml format
	dune build $(FRENCH_LAW_OCAML_LIB_DIR)/api.a

run_french_law_library_benchmark_ocaml: generate_french_law_library_ocaml
	dune exec --profile release $(FRENCH_LAW_OCAML_LIB_DIR)/bench.exe

run_french_law_library_ocaml_tests: build_french_law_library_ocaml
	dune exec $(FRENCH_LAW_OCAML_LIB_DIR)/law_source/unit_tests/run_tests.exe


#-----------------------------------------
# JS
#-----------------------------------------

FRENCH_LAW_JS_LIB_DIR=french_law/js

run_french_law_library_benchmark_js: build_french_law_library_js
	$(MAKE) -C $(FRENCH_LAW_JS_LIB_DIR) bench

#> build_french_law_library_js		: Builds the JS version of the OCaml French law library
build_french_law_library_js: generate_french_law_library_ocaml format
	dune build --profile release $(FRENCH_LAW_OCAML_LIB_DIR)/api_web.bc.js
	cp -f $(ROOT_DIR)/_build/default/$(FRENCH_LAW_OCAML_LIB_DIR)/api_web.bc.js $(FRENCH_LAW_JS_LIB_DIR)/french_law.js


#-----------------------------------------
# Python
#-----------------------------------------

FRENCH_LAW_PYTHON_LIB_DIR=french_law/python

$(FRENCH_LAW_PYTHON_LIB_DIR)/src/allocations_familiales.py:
	CATALA_OPTS="$(CATALA_OPTS) -O -t" $(MAKE) -C $(ALLOCATIONS_FAMILIALES_DIR) allocations_familiales.py
	cp -f $(ALLOCATIONS_FAMILIALES_DIR)/allocations_familiales.py $@

#> generate_french_law_library_python	: Generates the French law library Python sources from Catala
generate_french_law_library_python:\
	$(FRENCH_LAW_PYTHON_LIB_DIR)/src/allocations_familiales.py
	. $(FRENCH_LAW_PYTHON_LIB_DIR)/env/bin/activate ;\
	$(MAKE) -C $(FRENCH_LAW_PYTHON_LIB_DIR) format

#> type_french_law_library_python		: Types the French law library Python sources with mypy
type_french_law_library_python: generate_french_law_library_python
	. $(FRENCH_LAW_PYTHON_LIB_DIR)/env/bin/activate ;\
	$(MAKE) -C $(FRENCH_LAW_PYTHON_LIB_DIR) type

run_french_law_library_benchmark_python: type_french_law_library_python
	. $(FRENCH_LAW_PYTHON_LIB_DIR)/env/bin/activate ;\
	$(MAKE) -C $(FRENCH_LAW_PYTHON_LIB_DIR) bench

##########################################
# High-level test and benchmarks commands
##########################################

CATALA_OPTS?=
CLERK_OPTS?=

CATALA_BIN=_build/default/compiler/catala.exe
CLERK_BIN=_build/default/build_system/clerk.exe

CLERK=$(CLERK_BIN) --exe $(CATALA_BIN) \
	$(CLERK_OPTS) $(if $(CATALA_OPTS),--catala-opts=$(CATALA_OPTS),)


.FORCE:

test_suite: .FORCE
	@$(CLERK) test tests

test_examples: .FORCE
	@$(CLERK) test examples

test_clerk:
	@$(CLERK) test $(BUILD_SYSTEM_DIR)/tests

#> tests					: Run interpreter tests
tests: test_suite test_examples test_clerk

#> tests_ocaml				: Run OCaml unit tests for the Catala-generated code
tests_ocaml: run_french_law_library_ocaml_tests

#> bench_ocaml				: Run OCaml benchmarks for the Catala-generated code
bench_ocaml: run_french_law_library_benchmark_ocaml

#> bench_js				: Run JS benchmarks for the Catala-generated code
bench_js: run_french_law_library_benchmark_js

#> bench_python				: Run Python benchmarks for the Catala-generated code
bench_python: run_french_law_library_benchmark_python

##########################################
# Website assets
##########################################

grammar.html: $(COMPILER_DIR)/surface/parser.mly
	obelisk html -o $@ $<

catala.html: $(COMPILER_DIR)/utils/cli.ml
	dune exec $(COMPILER_DIR)/catala.exe -- --help=groff | man2html | sed -e '1,8d' \
	| tac | sed "1,20d" | tac > $@

#> website-assets				: Builds all the assets necessary for the Catala website
website-assets: doc literate_examples grammar.html catala.html js_build build_french_law_library_js

##########################################
# Misceallenous
##########################################

#> all					: Run all make commands
all: \
	dependencies build doc website-assets\
	tests \
	generate_french_law_library_ocaml build_french_law_library_ocaml \
	tests_ocaml bench_ocaml \
	build_french_law_library_js \
	bench_js \
	generate_french_law_library_python type_french_law_library_python\
	bench_python


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

#> help_clerk				: Display the clerk man page
help_clerk:
	$(CLERK_BIN) --help

#> help_catala				: Display the catala man page
help_catala:
	$(CATALA_BIN) --help

##########################################
# Special targets
##########################################
.PHONY: inspect clean all literate_examples english allocations_familiales pygments \
	install build_dev build doc format dependencies dependencies-ocaml \
	catala.html help
