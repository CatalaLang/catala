DEPS = bindlib ninja_utils ocolor otoml ubase json-data-encoding testo-diff

deps: deps-list
	for url in $(shell cat $<); do \
	  curl -L $$url | tar xv$$(if [ x$${url##*.} = "xtbz" ]; then echo "j"; else echo "z"; fi); \
	done
	echo '(lang dune 3.13)' > $(wildcard testo-*/dune-project)

deps-list:
	for pkg in $(DEPS); do \
	  opam show $$pkg --raw -f url.src; \
	done > $@
