TARGETS = syntax.pdf syntax_en.pdf syntax_fr.pdf

TYPST_FLAGS = --font-path fonts/ --ignore-system-fonts

all: syntax.pdf

%.pdf: $(wildcard *.typ)
	typst compile $(TYPST_FLAGS) $*.typ

clean:
	rm -rf _minted* $(TARGETS)

watch:
	typst watch $(TYPST_FLAGS) syntax.typ

.PHONY: all clean watch
