##########################################
# Variables
##########################################

LATEXMK?=latexmk

CURR_DIR=examples/$(shell basename $(shell pwd))/

CATALA=cd ../../; _build/default/compiler/catala.exe
CATALA_OPTS := $(CATALA_OPTS) --language=$(CATALA_LANG)

PLUGIN_DIR=_build/default/compiler/plugins

help : ../Makefile.common.mk
	@sed -n 's/^#> //p' $<

##########################################
# Targets
##########################################

#> SCOPE=<ScopeName> <target_file>.run		: Runs the interpeter for the scope of the file
%.run: %.catala_$(CATALA_LANG)
	@$(CATALA) Makefile $(CATALA_OPTS) $(CURR_DIR)$<
	$(CATALA) Interpret $(CATALA_OPTS) \
		-s $(SCOPE) \
		$(CURR_DIR)$<

#> <target_file>.ml				: Compiles the file to OCaml
%.ml: %.catala_$(CATALA_LANG)
	@$(CATALA) Makefile $(CATALA_OPTS) $(CURR_DIR)$<
	$(CATALA) OCaml $(CATALA_OPTS) \
		$(CURR_DIR)$<

#> <target_file>_api_web.ml			: Compiles the file to OCaml + generates the API web
%_api_web.ml: %.catala_$(CATALA_LANG)
	@$(CATALA) Makefile $(CATALA_OPTS) $(CURR_DIR)$<
	$(CATALA) api_web $(CATALA_OPTS) \
		--plugin-dir=$(PLUGIN_DIR) \
		$(CURR_DIR)$<

#> SCOPE=<ScopeName> <target_file>_api_web.ml	: Generates the JSON schema
%_schema.json: %.catala_$(CATALA_LANG)
	@$(CATALA) Makefile $(CATALA_OPTS) $(CURR_DIR)$<
	$(CATALA) json_schema $(CATALA_OPTS) \
		--plugin-dir=$(PLUGIN_DIR) \
		-s $(SCOPE) \
		$(CURR_DIR)$<

#> <target_file>.py				: Compiles the file to Python
%.py: %.catala_$(CATALA_LANG)
	@$(CATALA) Makefile $(CATALA_OPTS) $(CURR_DIR)$<
	$(CATALA) Python $(CATALA_OPTS) \
		$(CURR_DIR)$<

#> <target_file>.tex				: Weaves the file to LaTeX
%.tex: %.catala_$(CATALA_LANG)
	@$(CATALA) Makefile $(CATALA_OPTS) $(CURR_DIR)$<
	$(CATALA) LaTeX $(CATALA_OPTS) \
		--wrap \
		$(CURR_DIR)$<

#> <target_file>.pdf				: Weaves the file to PDF (via XeLaTeX)
%.pdf: %.tex
	cd $(@D) && $(LATEXMK) -g -xelatex -halt-on-error -shell-escape $(%F)

#> <target_file>.html				: Weaves the file to HTML
%.html: %.catala_$(CATALA_LANG)
	@$(CATALA) Makefile $(CATALA_OPTS) $(CURR_DIR)$<
	$(CATALA) HTML $(CATALA_OPTS) \
	--wrap \
	$(CURR_DIR)$<

%.spellok: %.catala_$(CATALA_LANG) ../whitelist.$(CATALA_LANG)
	aspell list --lang=$(CATALA_LANG) --mode=markdown --camel-case --add-wordlists=../whitelist.$(CATALA_LANG) <$< | tee "$<".errors
	@# list of mispelled words must be empty
	@test ! -s "$<".errors
	@touch $@


##########################################
# Misceallenous
##########################################

#> clean						: Removes intermediate files
clean:
	$(LATEXMK) -f -C $(SRC:.catala_$(CATALA_LANG)=.tex)
	rm -rf $(SRC:.catala_$(CATALA_LANG)=.tex) \
		$(SRC:.catala_$(CATALA_LANG)=.d) \
		_minted-$(SRC:.catala_$(CATALA_LANG)=) \
		$(SRC:.catala_$(CATALA_LANG)=.html) \
		$(SRC:.catala_$(CATALA_LANG)=.ml) \
		$(SRC:.catala_$(CATALA_LANG)=_api_web.ml) \
		$(SRC:.catala_$(CATALA_LANG)=_schema.json)

include $(wildcard $(SRC:.catala_$(CATALA_LANG)=.d))

.SECONDARY:
