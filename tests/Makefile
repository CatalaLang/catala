############################################
# Preamble
############################################

CATALA_OPTS?=
CLERK_OPTS?=--makeflags="$(MAKEFLAGS)"

CLERK=dune exec -- ../build_system/clerk.exe test \
	$(CLERK_OPTS) $(if $(CATALA_OPTS),--catala-opts=$(CATALA_OPTS),)

# Forces all the tests to be redone
.FORCE:

%.catala_en %.catala_fr %.catala_pl: .FORCE
	$(CLERK) $@

pass_all_tests:
	$(CLERK) .

reset_all_tests: CLERK_OPTS+=--reset
reset_all_tests:
	$(CLERK) .
