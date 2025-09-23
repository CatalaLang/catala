DEPS = bindlib dates_calc ninja_utils ocolor otoml ubase

deps-list:
	for pkg in $(DEPS); do \
	  opam show $$pkg --raw -f url.src; \
	done > $@

deps: deps-list
	for url in $(shell cat $<); do \
	  curl -L $$url | tar xvz; \
	done
