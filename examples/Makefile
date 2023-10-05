CATALA_OPTS?=
CLERK_OPTS?=--makeflags="$(MAKEFLAGS)"

CLERK=_build/default/build_system/clerk.exe test --exe "_build/default/compiler/catala.exe" \
	$(CLERK_OPTS) $(if $(CATALA_OPTS),--catala-opts=$(CATALA_OPTS),)

################################
# Running legislation unit tests
################################

pass_all_tests:
	@cd ..; $(CLERK) examples

reset_all_tests: CLERK_OPTS+=--reset
reset_all_tests:
	@cd ..; $(CLERK) examples

%.catala_en %.catala_fr %.catala_pl: .FORCE
    # Here we cd to the root of the Catala repository such that the paths \
	# displayed in error messages start with `examples/` uniformly.
	@cd ..; $(CLERK) examples/$@

.FORCE:

.PHONY: pass_all_tests reset_all_tests
