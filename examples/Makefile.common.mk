##########################################
# Variables
##########################################

LATEXMK=latexmk

CATALA=dune exec --no-print-director ../../src/catala/catala.exe -- \
	$(CATALA_OPTS) --language=$(CATALA_LANG)

##########################################
# Targets
##########################################

%.run: %.catala_$(CATALA_LANG)
	$(CATALA) Makefile $<
	$(CATALA) \
		Interpret \
		-s $(SCOPE) \
		$<

%.ml: %.catala_$(CATALA_LANG)
	$(CATALA) Makefile $<
	$(CATALA) \
		OCaml \
		$<


%.tex: %.catala_$(CATALA_LANG)
	$(CATALA) Makefile $<
	$(CATALA) \
		--wrap \
		LaTeX \
		$<

%.html: %.catala_$(CATALA_LANG) 
	$(CATALA) Makefile $<
	$(CATALA) \
	--wrap \
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
