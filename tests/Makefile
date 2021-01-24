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

CATALA=dune exec --no-buffer --no-print-director ../src/catala.exe -- Interpret $(CATALA_OPTS)

pass_tests: $(wildcard */output/*.out)

reset_tests: $(subst .out,.in,$(wildcard */output/*.out))

# Forces all the tests to be redone
.FORCE:

%.run: .FORCE
	$(CATALA) $(word 1,$(subst ., ,$*)).catala -s $(word 3,$(subst ., ,$*)) 

# Usage: make <test_dir>/output/<test_name>.catala.<scope_name>.out
# This rule runs the test and compares against the expected output. If the 
# Catala program is <test_dir>/good/<test_name>.catala or <test_dir>/bad/<test_name>.catala
# and the scope to run is <scope_name>, then the expected output should be in the file 
# <test_dir>/output/<test_name>.catala.<scope_name>.out
%.out: .FORCE
	@$(CATALA) --unstyled $(wildcard \
		$(word 1, $(subst /, , $*))/good/$(word 1, $(subst ., ,$(word 3, $(subst /, , $*))))* \
		$(word 1, $(subst /, , $*))/bad/$(word 1, $(subst ., ,$(word 3, $(subst /, , $*))))*) \
	-s $(word 3, $(subst ., ,$*)) 2>&1 | \
		colordiff -u -b $@ - || { echo "${RED}FAIL${RESET} ${PURPLE}$*${RESET}"; exit 1; }
	@echo "${GREEN}PASS${RESET} ${PURPLE}$*${RESET}"

# Usage: make <test_dir>/{good,bad}/<test_name>.catala.<scope_name>.in
# This rule runs the test <test_dir>/{good, bad}/<test_name>.catala, prints its output and 
# writes this output to the <test_dir>/output/<test_name>.catala.<scope_name>.out file
%.in: .FORCE
	@-$(CATALA) $(wildcard \
		$(word 1, $(subst /, , $*))/good/$(word 1, $(subst ., ,$(word 3, $(subst /, , $*))))* \
		$(word 1, $(subst /, , $*))/bad/$(word 1, $(subst ., ,$(word 3, $(subst /, , $*))))*) \
	-s $(word 3, $(subst ., ,$*))
	@-$(CATALA) --unstyled $(wildcard \
		$(word 1, $(subst /, , $*))/good/$(word 1, $(subst ., ,$(word 3, $(subst /, , $*))))* \
		$(word 1, $(subst /, , $*))/bad/$(word 1, $(subst ., ,$(word 3, $(subst /, , $*))))*) \
	-s $(word 3, $(subst ., ,$*)) \
		> $(word 1, $(subst /, , $*))/output/$(word 3, $(subst /, , $*)).out 2>&1
