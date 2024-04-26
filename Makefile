help : Makefile
	@sed -n 's/^#> //p' $<

ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

export DUNE_PROFILE ?= dev

# Export all variables to sub-make
export

##########################################
# Dependencies
##########################################

EXECUTABLES = groff python3 node npm ninja pandoc
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

PY_VENV_DIR = _python_venv

PY_VENV_ACTIVATE = . $(PY_VENV_DIR)/bin/activate;

# Rebuild when requirements change
$(PY_VENV_DIR): $(PY_VENV_DIR)/stamp

$(PY_VENV_DIR)/stamp: \
    runtimes/python/pyproject.toml \
    syntax_highlighting/en/pygments/pyproject.toml \
    syntax_highlighting/fr/pygments/pyproject.toml \
    syntax_highlighting/pl/pygments/pyproject.toml
	test -d $(PY_VENV_DIR) || python3 -m venv $(PY_VENV_DIR)
	$(PY_VENV_ACTIVATE) python3 -m pip install -U pip
	$(PY_VENV_ACTIVATE) python3 -m pip install -U \
	  -e runtimes/python \
	  -e syntax_highlighting/en/pygments \
	  -e syntax_highlighting/fr/pygments \
	  -e syntax_highlighting/pl/pygments
	touch $@

dependencies-python: $(PY_VENV_DIR)

build-runtime-python: dependencies-python
	$(PY_VENV_ACTIVATE) python3 -m pip install -U build
	$(PY_VENV_ACTIVATE) python -m build runtimes/python -o _build/python-runtime/

publish-runtime-python:
	$(PY_VENV_ACTIVATE) python3 -m pip install -U twine
	$(PY_VENV_ACTIVATE) python -m twine upload _build/python-runtime/*

#> dependencies				: Install the Catala OCaml, JS and Git dependencies
dependencies: dependencies-ocaml dependencies-python

dependencies-with-z3: dependencies-ocaml-with-z3 dependencies-python

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

# Just the base compiler as needed to run the tests
compiler: parser-messages
	dune build $(COMPILER_DIR)/catala.exe $(COMPILER_DIR)/plugins/ $(BUILD_SYSTEM_DIR)/clerk.exe

#> build					: Builds the Catala compiler
build: parser-messages format build_dev

#> js_build				: Builds the Web-compatible JS versions of the Catala compiler
js_build:
	dune build $(COMPILER_DIR)/catala.bc.js $(COMPILER_DIR)/catala_web_interpreter.bc.js

#> doc					: Generates the HTML OCaml documentation
doc:
	dune build @doc
	ln -sf $(PWD)/_build/default/_doc/_html/index.html doc/odoc.html

prepare-install:
	dune build @install --promote-install-files

install: prepare-install
	if [ x$$($(OPAM) --version) = "x2.1.5" ]; then \
	  $(OPAM) install . --working-dir; \
	else \
	  $(OPAM) install . --working-dir --assume-built; \
	fi
# `dune install` would work, but does a dirty install to the opam prefix without
# registering with opam.
# --assume-built is broken in 2.1.5

inst: prepare-install
	@opam custom-install \
	  catala.$$(_build/install/default/bin/catala --version) \
	  --solver=builtin-mccs+glpk -- \
	dune install catala
# This is better, but 'opam custom-install' is still an experimental plugin

#> runtimes				: Builds the OCaml and js_of_ocaml runtimes
runtimes:
	dune build runtimes/

#> plugins					: Builds the compiler backend plugins
plugins: runtimes
	dune build compiler/plugins/

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

pygmentize_%: $(PY_VENV_DIR)
	$(PY_VENV_ACTIVATE) python3 -m pip install syntax_highlighting/$*/pygments

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
# Extra documentation
##########################################

#> syntax					: Buils syntax sheet (requires latexmk and dejavu fonts)
syntax:
	$(MAKE) -C doc/syntax

##########################################
# High-level test and benchmarks commands
##########################################

CATALA_OPTS ?=
CLERK_OPTS ?=

CATALA_BIN=_build/default/$(COMPILER_DIR)/catala.exe
CLERK_BIN=_build/default/$(BUILD_SYSTEM_DIR)/clerk.exe

CLERK_TEST=$(CLERK_BIN) test --exe $(CATALA_BIN) \
	$(CLERK_OPTS) $(if $(CATALA_OPTS),--catala-opts=$(CATALA_OPTS),)


.FORCE:

unit-tests: .FORCE
	dune build @for-tests @runtest

#> test					: Run interpreter tests
test: .FORCE unit-tests
	$(CLERK_TEST) tests doc

tests: test

TEST_FLAGS_LIST = ""\
-O \
--lcalc \
--lcalc,--avoid-exceptions,-O

# Does not include running dune (to avoid duplication when run among bigger rules)
testsuite-base: .FORCE
	@for F in $(TEST_FLAGS_LIST); do \
	  echo >&2; \
	  [ -z "$$F" ] || echo ">> RE-RUNNING TESTS WITH FLAGS: $$F" >&2; \
	  $(CLERK_TEST) tests --test-flags="$$F" || break; \
	done

#> testsuite				: Run interpreter tests over a selection of configurations
testsuite: unit-tests
	$(MAKE) testsuite-base

#> reset-tests				: Update the expected test results from current run
reset-tests: .FORCE $(CLERK_BIN)
	$(CLERK_TEST) tests doc --reset

tests/%: .FORCE
	$(CLERK_TEST) test $@

##########################################
# Website assets
##########################################

# Note: these are already built by the @doc dune alias
# (and therefore the doc target here)
WEBSITE_ASSETS = grammar.html catala.html clerk.html

WEBSITE_ASSETS_EXAMPLES = \
  tutorial_en/tutorial_en.html \
  tutoriel_fr/tutoriel_fr.html \
  us_tax_code/us_tax_code.html \
  allocations_familiales/Allocations_familiales.html \
  allocations_familiales/Allocations_familiales_schema.json \
  aides_logement/Aides_logement.html \
  aides_logement/Aides_logement_schema.json

WEBSITE_ASSETS_ALL = $(WEBSITE_ASSETS) $(addprefix catala-examples.tmp/,$(WEBSITE_ASSETS_EXAMPLES))

website-assets-base: build
	$(call local_tmp_clone,catala-examples) && \
	$(MAKE) -C catala-examples.tmp \
	  CATALA=../$(CATALA_BIN) \
	  CLERK=../$(CLERK_BIN) \
	  BUILD=../_build/default \
	  $(addprefix ../_build/default/,$(WEBSITE_ASSETS_EXAMPLES))
	dune build $(addprefix _build/default/,$(WEBSITE_ASSETS_ALL)) $(WEBSITE_ASSETS)

website-assets.tar: website-assets-base
	tar cf $@ $(foreach file,$(WEBSITE_ASSETS_ALL),-C $(CURDIR)/$(dir _build/default/$(file)) $(notdir $(file)))

#> website-assets				: Builds all the assets necessary for the Catala website
website-assets: website-assets.tar

##########################################
# Miscellaneous
##########################################

#> all					: Run all make commands
all: \
	build js_build doc \
	tests \
	runtimes \
	plugins

BRANCH = $(shell git branch --show-current 2>/dev/null || echo master)

# Attempt a clone of the named CatalaLang repo into <name>.tmp, using local git
# objects in ../<name> if available, the branch with the same name as the
# current branch if it exists (master otherwise), and falling back to a local
# clone of ../<name> if the network is not available. The temp dir is removed
# when the shell terminates, so this must be run in the same "Makefile line" as
# its usage.
local_tmp_clone = { \
  rm -rf $1.tmp && \
  trap "rm -rf $1.tmp" EXIT && \
  git clone https://github.com/CatalaLang/$1 \
    --depth 1 --reference-if-able ../$1 \
    $1.tmp -b $(BRANCH) || \
  git clone https://github.com/CatalaLang/$1 \
    --depth 1 --reference-if-able ../$1 \
    $1.tmp || \
  git clone -s ../$1 $1.tmp -b $(BRANCH) || \
  git clone -s ../$1 $1.tmp -b master; \
}

test_title = printf "\n\#             \e[33m===========  \e[1m%-30s  \e[2m===========\e[m             \n"

#> alltest					: Runs more extensive tests, including the examples and french-law. Use before push!
alltest: dependencies-python
	@export DUNE_PROFILE=check OCAMLPATH=$(CURDIR)/_build/install/default/lib && \
	$(test_title) "Local build and unit tests" && \
	dune build @update-parser-messages @install @runtest && \
	$(test_title) "Local testsuite" && \
	$(MAKE) testsuite && \
	$(test_title) "Running catala-examples" && \
	$(call local_tmp_clone,catala-examples) && \
	$(MAKE) -C catala-examples.tmp \
	  CATALA=$(CURDIR)/_build/install/default/bin/catala \
	  CLERK=$(CURDIR)/_build/install/default/bin/clerk \
	  BUILD=../_build/default \
	  all testsuite local-install && \
	$(test_title) "Running french-law tests" && \
	$(call local_tmp_clone,french-law) && \
	touch french-law.tmp/dune-workspace && \
	$(MAKE) -C french-law.tmp \
	  OCAMLPATH=$(CURDIR)/_build/install/default/lib \
	  PY_VENV_DIR=$(ROOT_DIR)/_python_venv \
	  dependencies \
	  bench_ocaml \
	  bench_js \
	  bench_python && \
	printf "\n#             \e[42;30m[ ALL TESTS PASSED ]\e[m             \e[32m☺\e[m\n" || \
	{ printf "\n#             \e[41;30m[   TESTS FAILED   ]\e[m             \e[31m☹\e[m\n" ; exit 1; }

#> clean					: Clean build artifacts
clean:
	dune clean
	rm -rf artifacts

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
.PHONY: inspect clean all english alltest pygments install build_dev build doc	\
	format dependencies dependencies-ocaml catala.html help parser-messages	\
	plugins website-assets.tar website-assets-base
