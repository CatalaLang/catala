DEPS = bindlib ninja_utils ocolor otoml ubase json-data-encoding

deps: deps-list
	for url in $(shell cat $<); do \
	  curl -L $$url | tar xvz; \
	done

deps-list:
	for pkg in $(DEPS); do \
	  opam show $$pkg --raw -f url.src; \
	done > $@
