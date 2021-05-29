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

CATALA_OPTS?=

CATALA=../_build/default/src/catala/catala.exe Interpret $(CATALA_OPTS)

pass_tests: $(wildcard */*/output/*.out)

reset_tests: $(subst output/,,$(subst .out,.in,$(wildcard */*/output/*.out)))

# Forces all the tests to be redone
.FORCE:

# FIXME: .catala should be replaced by the corresponding regexp.
%.run: .FORCE
	$(CATALA) $(word 1,$(subst ., ,$*)).catala -s $(word 3,$(subst ., ,$*))

# Usage: make <test_dir>/output/<test_name>.catala_<language>.<scope_name>.out
# This rule runs the test and compares against the expected output. If the
# Catala program is <test_dir>/<test_name>.catala_<language>
# and the scope to run is <scope_name>, then the expected output should be in the file
# <test_dir>/output/<test_name>.catala_<language>.<scope_name>.out
%.out: .FORCE
	@$(CATALA) --unstyled \
  $(word 1, $(subst /, , $*))/$(word 2, $(subst /, , $*))/$(word 1, $(subst ., ,$(word 4, $(subst /, , $*)))).$(word 2, $(subst ., , $*)) \
	-s $(word 3, $(subst ., ,$*)) 2>&1 | \
		colordiff -u -b $@ - || { echo "${RED}FAIL${RESET} ${PURPLE}$*${RESET}"; exit 1; }
	@echo "${GREEN}PASS${RESET} ${PURPLE}$*${RESET}"

# Usage: make <test_dir>/<test_name>.catala_<language>.<scope_name>.in
# This rule runs the test <test_dir>/<test_name>.catala_<language>, prints its output and
# writes this output to the <test_dir>/output/<test_name>.catala_<language>.<scope_name>.out file
%.in: .FORCE
	@-$(CATALA) $(word 1,$(subst ., ,$*)).$(word 2, $(subst ., , $*)) -s $(word 3,$(subst ., ,$*))
	@-$(CATALA) --unstyled $(word 1,$(subst ., ,$*)).$(word 2, $(subst ., , $*)) -s $(word 3,$(subst ., ,$*)) \
		> $(dir $*)output/$(lastword $(subst /, ,$*)).out 2>&1
