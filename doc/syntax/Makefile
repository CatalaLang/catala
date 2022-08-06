syntax.pdf: syntax.tex
	latexmk -pdf -halt-on-error -shell-escape -pvc syntax.tex

clean:
	latexmk -f -C syntax.tex

.PRECIOUS .SECONDARY: syntax.pdf
