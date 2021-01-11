BLACK        := $(shell tput -Txterm setaf 0)
RED          := $(shell tput -Txterm setaf 1)
GREEN        := $(shell tput -Txterm setaf 2)
YELLOW       := $(shell tput -Txterm setaf 3)
LIGHTPURPLE  := $(shell tput -Txterm setaf 4)
PURPLE       := $(shell tput -Txterm setaf 5)
BLUE         := $(shell tput -Txterm setaf 6)
WHITE        := $(shell tput -Txterm setaf 7)

RESET := $(shell tput -Txterm sgr0)

################################
# Running legislation unit tests
################################

# Usage `make <name_of_example_folder>.<name_of_test_file>.<name_of_test_scope>.run`
# This Makefile rule assumes the following directory structure:
# foo_example
#     tests/
#	      foo_test_file1.catala
#         foo_test_file2.catala	  
#         ...
#     foo_implem.catala
#     ...
%.run: .FORCE
	@SCOPE="$(word 3,$(subst ., ,$*))" $(MAKE) --no-print-directory -C \
		$(word 1,$(subst ., ,$*)) tests/$(word 2,$(subst ., ,$*)).run \
	> /dev/null || { echo "${RED}FAIL${RESET} ${PURPLE}$@${RESET}"; exit 1; }
	@echo "${GREEN}PASS${RESET} ${PURPLE}$@${RESET}"

TEST_FILES?=$(wildcard */tests/*.catala*)

TEST_FILES_SCOPES_EN=$(foreach TEST_FILE,$(TEST_FILES),\
	$(foreach TEST_SCOPE,\
		$(shell grep -Po "declaration scope [^:]*" $(TEST_FILE) | cut -d " " -f 3), \
		$(word 1,$(subst /, ,$(TEST_FILE))).$(word 1,$(subst ., ,$(word 3,$(subst /, ,$(TEST_FILE))))).$(TEST_SCOPE).run \
	) \
)

TEST_FILES_SCOPES_FR=$(foreach TEST_FILE,$(TEST_FILES),\
	$(foreach TEST_SCOPE,\
		$(shell grep -Po "déclaration champ d'application [^:]*" $(TEST_FILE) | cut -d " " -f 4), \
		$(word 1,$(subst /, ,$(TEST_FILE))).$(word 1,$(subst ., ,$(word 3,$(subst /, ,$(TEST_FILE))))).$(TEST_SCOPE).run \
	) \
)

tests: $(TEST_FILES_SCOPES_EN) $(TEST_FILES_SCOPES_FR)

.FORCE:
