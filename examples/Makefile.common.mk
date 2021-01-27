##########################################
# Variables
##########################################

LATEXMK=latexmk

PYGMENTIZE_FR=../../syntax_highlighting/fr/pygments/pygments/env/bin/pygmentize
PYGMENTIZE_EN=../../syntax_highlighting/en/pygments/pygments/env/bin/pygmentize

CATALA=dune exec --no-print-director ../../src/catala.exe -- $(CATALA_OPTS) --language=$(CATALA_LANG)

LEGIFRANCE_CATALA=dune exec ../../src/legifrance_catala.exe --

CATALA_EXE=../../_build/default/src/catala.exe

ifeq ($(CATALA_LANG),fr)
	PYGMENTIZE=$(PYGMENTIZE_FR)
endif
ifeq ($(CATALA_LANG),en)
	PYGMENTIZE=$(PYGMENTIZE_EN)
endif

##########################################
# Targets
##########################################

%.run: %.catala_$(CATALA_LANG) $(CATALA_EXE)
	@$(CATALA) Makefile $<
	@$(CATALA) \
		Interpret \
		-s $(SCOPE) \
		$<

%.ml: %.catala_$(CATALA_LANG) $(CATALA_EXE)
	@$(CATALA) Makefile $<
	@$(CATALA) \
		OCaml \
		-s $(SCOPE) \
		$<


%.tex: %.catala_$(CATALA_LANG) $(CATALA_EXE)
	@$(CATALA) Makefile $<
	$(CATALA) \
		--wrap \
		--pygmentize=$(PYGMENTIZE) \
		LaTeX \
		$<

%.html: %.catala_$(CATALA_LANG) $(CATALA_EXE)
	@$(CATALA) Makefile $<
	$(CATALA) \
	--wrap \
	--pygmentize=$(PYGMENTIZE) \
	HTML \
	$<

%.pdf: %.tex
	cd $(@D) && $(LATEXMK) -g -pdf -halt-on-error -shell-escape $(%F)

##########################################
# Misceallenous
##########################################

clean:
	$(LATEXMK) -f -C $(SRC:.catala_$(CATALA_LANG)=.tex)
	rm -rf $(SRC:.catala_$(CATALA_LANG)=.tex) \
		$(SRC:.catala_$(CATALA_LANG)=.d) \
		_minted-$(SRC:.catala_$(CATALA_LANG)=) \
		$(SRC:.catala_$(CATALA_LANG)=.html)

include $(wildcard $(SRC:.catala_$(CATALA_LANG)=.d))

.SECONDARY:
