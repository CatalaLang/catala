CATALA_BIN=../_build/default/compiler/catala.exe
CLERK_BIN=../_build/default/build_system/clerk.exe

CLERK=$(CLERK_BIN) --exe $(CATALA_BIN)

simple_test:
	@$(CLERK) test tests

reset:
	@$(CLERK) test tests -r

test: simple_test reset
