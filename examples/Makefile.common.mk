##########################################
# Variables
##########################################

LATEXMK?=latexmk

CATALA=../../_build/default/compiler/catala.exe \
	$(CATALA_OPTS) --language=$(CATALA_LANG)

help : ../Makefile.common.mk
	@sed -n 's/^#> //p' $<

##########################################
# Targets
##########################################

#> SCOPE=<ScopeName> <target_file>.run	: Runs the interpeter for the scope of the file
%.run: %.catala_$(CATALA_LANG)
	@$(CATALA) Makefile $<
	$(CATALA) \
		Interpret \
		-s $(SCOPE) \
		$<

#> <target_file>.ml			: Compiles the file to OCaml
%.ml: %.catala_$(CATALA_LANG)
	@$(CATALA) Makefile $<
	$(CATALA) \
		OCaml \
		$<

#> <target_file>.py			: Compiles the file to Python
%.py: %.catala_$(CATALA_LANG)
	@$(CATALA) Makefile $<
	$(CATALA) \
		Python \
		$<

#> <target_file>.tex			: Weaves the file to LaTeX
%.tex: %.catala_$(CATALA_LANG)
	$(CATALA) Makefile $<
	$(CATALA) \
		--wrap \
		LaTeX \
		$<

#> <target_file>.pdf			: Weaves the file to PDF (via LaTeX)
%.pdf: %.tex
	cd $(@D) && $(LATEXMK) -g -pdf -halt-on-error -shell-escape $(%F)

#> <target_file>.html			: Weaves the file to HTML
%.html: %.catala_$(CATALA_LANG)
	@$(CATALA) Makefile $<
	$(CATALA) \
	--wrap \
	HTML \
	$<


##########################################
# Misceallenous
##########################################

#> clean				: Removes intermediate files
clean:
	$(LATEXMK) -f -C $(SRC:.catala_$(CATALA_LANG)=.tex)
	rm -rf $(SRC:.catala_$(CATALA_LANG)=.tex) \
		$(SRC:.catala_$(CATALA_LANG)=.d) \
		_minted-$(SRC:.catala_$(CATALA_LANG)=) \
		$(SRC:.catala_$(CATALA_LANG)=.html) \
		$(SRC:.catala_$(CATALA_LANG)=.ml) \

include $(wildcard $(SRC:.catala_$(CATALA_LANG)=.d))

.SECONDARY:
