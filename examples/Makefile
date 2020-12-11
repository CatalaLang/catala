################################
# Running legislation unit tests
################################


# Usage `make <name_of_example_folder>.<name_of_test_file>.<name_of_test_scope>`
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
	> /dev/null || { echo "[FAIL $@]"; exit 1; }
	@echo "[PASS $@]"

TEST_FILES=$(wildcard */tests/*.catala*)

TEST_FILES_SCOPES=$(foreach TEST_FILE,$(TEST_FILES),\
	$(foreach TEST_SCOPE,\
		$(shell grep -Po "declaration scope [^:]*" $(TEST_FILE) | cut -d " " -f 3), \
		$(word 1,$(subst /, ,$(TEST_FILE))).$(word 1,$(subst ., ,$(word 3,$(subst /, ,$(TEST_FILE))))).$(TEST_SCOPE).run \
	) \
)

test_examples: $(TEST_FILES_SCOPES)

.FORCE: