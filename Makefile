help : Makefile
	@sed -n 's/^#> //p' $<

ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

export DUNE_PROFILE ?= release

# Export all variables to sub-make
export

##########################################
# Dependencies
##########################################

EXECUTABLES = groff python3 colordiff node pygmentize node npm ninja pandoc
K := $(foreach exec,$(EXECUTABLES),\
        $(if $(shell which $(exec)),some string,$(warning [WARNING] No "$(exec)" executable found. \
				Please install this executable for everything to work smoothly)))

OPAM = opam --cli=2.1

dependencies-ocaml:
	$(OPAM) pin . --no-action
	OPAMVAR_cataladevmode=1 $(OPAM) install . --with-doc --with-test --update-invariant --depext-only
	OPAMVAR_cataladevmode=1 $(OPAM) install . --with-doc --with-test --update-invariant --deps-only

dependencies-ocaml-with-z3:
	$(OPAM) pin . --no-action
	OPAMVAR_cataladevmode=1 OPAMVAR_catalaz3mode=1 $(OPAM) install . --with-doc --with-test --update-invariant --depext-only
	OPAMVAR_cataladevmode=1 OPAMVAR_catalaz3mode=1 $(OPAM) install . --with-doc --with-test --update-invariant --deps-only

dependencies-js:
	$(MAKE) -C $(FRENCH_LAW_JS_LIB_DIR) dependencies

#> dependencies				: Install the Catala OCaml, JS and Git dependencies
dependencies: dependencies-ocaml dependencies-js

dependencies-with-z3: dependencies-ocaml-with-z3 dependencies-js

##########################################
# Catala compiler rules
##########################################

COMPILER_DIR=compiler
BUILD_SYSTEM_DIR=build_system

#> build_dev				: Builds the Catala compiler, without formatting code
build_dev: parser-messages
	dune build \
		$(COMPILER_DIR)/catala.exe \
		$(COMPILER_DIR)/plugins/ \
		$(BUILD_SYSTEM_DIR)/clerk.exe

#> build					: Builds the Catala compiler
build: parser-messages format build_dev

#> js_build				: Builds the Web-compatible JS versions of the Catala compiler
js_build:
	dune build $(COMPILER_DIR)/catala.bc.js
	dune build $(COMPILER_DIR)/catala_web_interpreter.bc.js

#> doc					: Generates the HTML OCaml documentation
doc:
	dune build @doc
	ln -sf $(PWD)/_build/default/_doc/_html/index.html doc/odoc.html

install:
	dune build @install

#> runtimes				: Builds the OCaml and js_of_ocaml runtimes
runtimes:
	dune build runtimes/

#> plugins					: Builds the compiler backend plugins
plugins: runtimes
	dune build compiler/plugins/
	@echo "define CATALA_PLUGINS=_build/default/compiler/plugins to test the plugins"

##########################################
# Rules related to promoted files
##########################################

check-promoted:
	dune build @update-parser-messages @fmt

compiler/surface/parser.messages: compiler/surface/tokens.mly compiler/surface/parser.mly
	-dune build @update-parser-messages --auto-promote
parser-messages: compiler/surface/parser.messages

format:
	-dune build @fmt --auto-promote 2>/dev/null

##########################################
# Syntax highlighting rules
##########################################

SYNTAX_HIGHLIGHTING_FR=${CURDIR}/syntax_highlighting/fr
SYNTAX_HIGHLIGHTING_EN=${CURDIR}/syntax_highlighting/en
SYNTAX_HIGHLIGHTING_PL=${CURDIR}/syntax_highlighting/pl

pygmentize_fr: $(SYNTAX_HIGHLIGHTING_FR)/set_up_pygments.sh
	chmod +x $<
	$<

pygmentize_en: $(SYNTAX_HIGHLIGHTING_EN)/set_up_pygments.sh
	chmod +x $<
	$<

pygmentize_pl: $(SYNTAX_HIGHLIGHTING_PL)/set_up_pygments.sh
	chmod +x $<
	$<

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
AIDES_LOGEMENT_DIR=$(EXAMPLES_DIR)/aides_logement
CODE_GENERAL_IMPOTS_DIR=$(EXAMPLES_DIR)/code_general_impots
US_TAX_CODE_DIR=$(EXAMPLES_DIR)/us_tax_code
TUTORIAL_EN_DIR=$(EXAMPLES_DIR)/tutorial_en
TUTORIEL_FR_DIR=$(EXAMPLES_DIR)/tutoriel_fr
POLISH_TAXES_DIR=$(EXAMPLES_DIR)/polish_taxes

literate_aides_logement: build
	$(MAKE) -C $(AIDES_LOGEMENT_DIR) aides_logement.tex
	$(MAKE) -C $(AIDES_LOGEMENT_DIR) aides_logement.html

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
	literate_us_tax_code literate_tutorial_en literate_tutoriel_fr \
	literate_polish_taxes literate_aides_logement

##########################################
# French law library
##########################################

#-----------------------------------------
# OCaml
#-----------------------------------------

FRENCH_LAW_OCAML_LIB_DIR = french_law/ocaml

FRENCH_LAW_LIBRARY_OCAML = \
  $(FRENCH_LAW_OCAML_LIB_DIR)/law_source/allocations_familiales_api_web.ml \
  $(FRENCH_LAW_OCAML_LIB_DIR)/law_source/unit_tests/tests_allocations_familiales.ml \
  $(FRENCH_LAW_OCAML_LIB_DIR)/law_source/aides_logement_api_web.ml

$(addprefix _build/default/,$(FRENCH_LAW_LIBRARY_OCAML)) :
	dune build $@

#> generate_french_law_library_ocaml	: Generates the French law library OCaml sources from Catala
generate_french_law_library_ocaml:
	dune build $(FRENCH_LAW_LIBRARY_OCAML)

#> build_french_law_library_ocaml		: Builds the OCaml French law library
build_french_law_library_ocaml:
	dune build $(FRENCH_LAW_OCAML_LIB_DIR)/api.a

run_french_law_library_benchmark_ocaml:
	dune exec --profile release $(FRENCH_LAW_OCAML_LIB_DIR)/bench.exe

run_french_law_library_ocaml_tests:
	dune exec $(FRENCH_LAW_OCAML_LIB_DIR)/law_source/unit_tests/run_tests.exe

#-----------------------------------------
# JSON schemas
#-----------------------------------------

JSON_SCHEMAS = \
  $(AIDES_LOGEMENT_DIR)/aides_logement_schema.json \
  $(ALLOCATIONS_FAMILIALES_DIR)/allocations_familiales_schema.json

#> generate_french_law_json_schemas	: Generates the French law library JSON schemas
$(addprefix _build/default/,$(JSON_SCHEMAS)):
	dune build $@

generate_french_law_json_schemas:
	dune build $(JSON_SCHEMAS)

#-----------------------------------------
# JS
#-----------------------------------------

FRENCH_LAW_JS_LIB_DIR=french_law/js

run_french_law_library_benchmark_js: build_french_law_library_js
	$(MAKE) -C $(FRENCH_LAW_JS_LIB_DIR) bench

#> build_french_law_library_js		: Builds the JS version of the OCaml French law library
build_french_law_library_js:
	dune build $(FRENCH_LAW_JS_LIB_DIR)/french_law.js

#> build_french_law_library_web_api	: Builds the web API of the French law library
build_french_law_library_web_api: build_french_law_library_js generate_french_law_json_schemas

#-----------------------------------------
# Python
#-----------------------------------------

FRENCH_LAW_PYTHON_LIB_DIR=french_law/python

FRENCH_LAW_LIBRARY_PYTHON = \
  $(FRENCH_LAW_PYTHON_LIB_DIR)/src/allocations_familiales.py \
  $(FRENCH_LAW_PYTHON_LIB_DIR)/src/aides_logement.py

PY_VIRTUALENV = $(FRENCH_LAW_PYTHON_LIB_DIR)/env/bin/activate

$(PY_VIRTUALENV):
	@$(if $(wildcard $(PY_VIRTUALENV)),,$(error "Python virtualenv not initialised, you need to run $(FRENCH_LAW_PYTHON_LIB_DIR)/setup_env.sh"))

$(FRENCH_LAW_LIBRARY_PYTHON):
	dune build $@

#> generate_french_law_library_python	: Generates the French law library Python sources from Catala
generate_french_law_library_python:
	dune build $(FRENCH_LAW_LIBRARY_PYTHON)

#> type_french_law_library_python		: Types the French law library Python sources with mypy
type_french_law_library_python: $(PY_VIRTUALENV) \
  generate_french_law_library_python
	. $(PY_VIRTUALENV) ;\
	$(MAKE) -C $(FRENCH_LAW_PYTHON_LIB_DIR) type

run_french_law_library_benchmark_python: $(PY_VIRTUALENV) \
  type_french_law_library_python
	. $(PY_VIRTUALENV) ;\
	$(MAKE) -C $(FRENCH_LAW_PYTHON_LIB_DIR) bench

##########################################
# High-level test and benchmarks commands
##########################################

CATALA_OPTS?=
CLERK_OPTS?=--makeflags="$(MAKEFLAGS)"

CATALA_BIN=_build/default/compiler/catala.exe
CLERK_BIN=_build/default/build_system/clerk.exe

CLERK=$(CLERK_BIN) --exe $(CATALA_BIN) \
	$(CLERK_OPTS) $(if $(CATALA_OPTS),--catala-opts=$(CATALA_OPTS),)


.FORCE:

test_suite: .FORCE
	OCAMLRUNPARAM= $(CLERK) test tests

test_examples: .FORCE
	OCAMLRUNPARAM= $(CLERK) test examples

#> tests					: Run interpreter tests
tests: test_suite test_examples

#> tests_ocaml				: Run OCaml unit tests for the Catala-generated code
tests_ocaml: run_french_law_library_ocaml_tests

#> bench_ocaml				: Run OCaml benchmarks for the Catala-generated code
bench_ocaml: run_french_law_library_benchmark_ocaml

#> bench_js				: Run JS benchmarks for the Catala-generated code
bench_js: run_french_law_library_benchmark_js

#> bench_python				: Run Python benchmarks for the Catala-generated code
bench_python: run_french_law_library_benchmark_python

tests/%: .FORCE
	@$(MAKE) -C tests $*

##########################################
# Website assets
##########################################

WEBSITE_ASSETS = grammar.html catala.html

$(addprefix _build/default/,$(WEBSITE_ASSETS)):
	dune build $@

#> website-assets				: Builds all the assets necessary for the Catala website
website-assets: js_build literate_examples build_french_law_library_web_api doc
	dune build $(WEBSITE_ASSETS)

##########################################
# Misceallenous
##########################################

#> all					: Run all make commands
all: \
	build js_build doc \
	tests \
	runtimes \
	plugins \
	generate_french_law_library_ocaml build_french_law_library_ocaml \
	tests_ocaml bench_ocaml \
	build_french_law_library_js \
	bench_js \
	generate_french_law_library_python type_french_law_library_python \
	bench_python \
	website-assets


#> clean					: Clean build artifacts
clean:
	dune clean
	rm -rf artifacts
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
.PHONY: inspect clean all literate_examples english allocations_familiales	\
	pygments install build_dev build doc format dependencies		\
	dependencies-ocaml catala.html help parser-messages plugins		\
	generate_french_law_json_schemas generate_french_law_library_python	\
	generate_french_law_library_ocaml					\
	run_french_law_library_benchmark_python					\
	run_french_law_library_benchmark_js run_french_law_library_ocaml_tests	\
	build_french_law_library_js build_french_law_library_web_api		\
	build_french_law_library_ocaml
