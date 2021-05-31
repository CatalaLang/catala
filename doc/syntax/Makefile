syntax.pdf: syntax.tex
	latexmk -pdf -halt-on-error -shell-escape -pvc syntax.tex

clean:
	latexmk -C syntax.tex

.PRECIOUS .SECONDARY: syntax.pdf
