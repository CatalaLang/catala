LATEXMK=latexmk

PYGMENTIZE=../../syntax_highlighting/pygments/pygments/env/bin/pygmentize


SRC = allocations_familiales.catala

%.tex: %.catala
	dune exec ../../src/main.exe -- Makefile $<
	dune exec ../../src/main.exe --\
		--debug \
		--wrap_latex \
		--pygmentize=$(PYGMENTIZE)\
		LaTeX \
		$<

%.pdf: %.tex
	cd $(@D) && $(LATEXMK) -g -pdf -halt-on-error -shell-escape $(%F)

clean:
	$(LATEXMK) -f -C $(SRC:.catala=.tex)
	rm -rf $(SRC:.catala=.tex) $(SRC:.catala=.d) _minted-$(SRC:.catala=)

include $(wildcard $(SRC:.catala=.d))
