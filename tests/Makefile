############################################
# Preamble
############################################

CATALA_OPTS?=
CLERK_OPTS?=--makeflags="$(MAKEFLAGS)"

CLERK=_build/default/build_system/clerk.exe test --exe "_build/default/compiler/catala.exe" \
	$(CLERK_OPTS) $(if $(CATALA_OPTS),--catala-opts=$(CATALA_OPTS),)

# Forces all the tests to be redone
.FORCE:

%.catala_en %.catala_fr %.catala_pl: .FORCE
    # Here we cd to the root of the Catala repository such that the paths \
	# displayed in error messages start with `tests/` uniformly.
	@cd ..; $(CLERK) tests/$@

pass_all_tests:
	@cd ..; $(CLERK) tests

reset_all_tests: CLERK_OPTS+=--reset
reset_all_tests:
	@cd ..; $(CLERK) tests
