CATALA_OPTS?=
CLERK_OPTS?=--makeflags="$(MAKEFLAGS)"

CLERK=_build/default/build_system/clerk.exe test --exe "_build/default/compiler/catala.exe" \
	$(CLERK_OPTS) $(if $(CATALA_OPTS),--catala-opts=$(CATALA_OPTS),)

CLERK_OPTS += -I base_mensuelle_allocations_familiales -I smic -I allocations_familiales -I prologue_france -I prestations_familiales -I aides_logement


################################
# Running legislation unit tests
################################

pass_all_tests:
	@$(CLERK) test .

reset_all_tests:
	@$(CLERK) test . --reset

%.catala_en %.catala_fr %.catala_pl: .FORCE
	@$(CLERK) test $@

.FORCE:

.PHONY: pass_all_tests reset_all_tests
