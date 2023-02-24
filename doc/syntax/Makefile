syntax.pdf: syntax_en.pdf syntax_fr.pdf
	cpdf $^ -o $@

%.pdf: %.tex logo.pdf
	latexmk -pdf -halt-on-error -shell-escape $<

clean:
	latexmk -f -C *.tex
	rm -rf _minted*

.PRECIOUS .SECONDARY: syntax.pdf syntax_en.pdf syntax_fr.pdf
