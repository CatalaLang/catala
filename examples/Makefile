CATALA_OPTS?=
CLERK_OPTS?=

CLERK=../_build/default/build_system/clerk.exe --exe "../_build/default/compiler/catala.exe" \
	$(CLERK_OPTS) $(if $(CATALA_OPTS),--catala-opts=$(CATALA_OPTS),) test

################################
# Running legislation unit tests
################################


%.catala_en %.catala_fr %.catala_pl: .FORCE
    # Here we cd to the root of the Catala repository such that the paths \
	# displayed in error messages start with `examples/` uniformly.
	@cd ..;$(CLERK) $@

TEST_FILES?=$(wildcard */tests/*.catala*)

pass_tests: $(TEST_FILES)
reset_tests: CLERK_OPTS+=--reset
reset_tests: $(TEST_FILES)

.FORCE:
