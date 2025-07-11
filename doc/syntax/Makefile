TARGETS=syntax.pdf syntax_en.pdf syntax_fr.pdf

all: syntax.pdf

%.pdf: %.typ
	typst compile $<

clean:
	rm -rf _minted* $(TARGETS)
