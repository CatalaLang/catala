############################################
# Preamble
############################################

BLACK        := $(shell tput -Txterm setaf 0)
RED          := $(shell tput -Txterm setaf 1)
GREEN        := $(shell tput -Txterm setaf 2)
YELLOW       := $(shell tput -Txterm setaf 3)
LIGHTPURPLE  := $(shell tput -Txterm setaf 4)
PURPLE       := $(shell tput -Txterm setaf 5)
BLUE         := $(shell tput -Txterm setaf 6)
WHITE        := $(shell tput -Txterm setaf 7)

RESET := $(shell tput -Txterm sgr0)

CATALA_OPTS?=""

CLERK_OPTS?=

CLERK=../_build/default/build_system/clerk.exe --exe "../_build/default/compiler/catala.exe" \
	$(CLERK_OPTS) --catala-opts=$(CATALA_OPTS) test

pass_tests: $(wildcard */*/*.catala_*)

reset_tests: CLERK_OPTS+=--reset
reset_tests: $(wildcard */*/*.catala_*)


# Forces all the tests to be redone
.FORCE:

%.catala_en %.catala_fr %.catala_pl: .FORCE
	@$(CLERK) $@
