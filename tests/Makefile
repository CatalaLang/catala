############################################
# Preamble
############################################

CATALA=dune exec --no-print-director ../src/catala.exe -- Interpret

tests: $(wildcard */*.catala)

# Forces all the tests to be redone
.FORCE:
*/*.catala: .FORCE

interpret_with_scope =\
  -$(CATALA) $(if $(filter $(1),nv),,-l $(1)) $@ -s $(2)

interpret_with_scope_and_compare =\
	$(CATALA) $(if $(filter $(1),nv),,-l $(1)) --unstyled $@ -s $(2)  2>&1 | \
		colordiff -u -b $@.$(2).out -

############################################
# Tests have to be registered here
############################################

test_bool/test_bool.catala:
	$(call interpret_with_scope_and_compare,nv,TestBool)
test_func/func.catala:
	$(call interpret_with_scope,nv,S)
test_scope/scope.catala:
	$(call interpret_with_scope_and_compare,nv,A)
test_scope/sub_scope.catala:
	$(call interpret_with_scope_and_compare,nv,A)
	$(call interpret_with_scope_and_compare,nv,B)
test_scope/sub_sub_scope.catala:
	$(call interpret_with_scope_and_compare,nv,A)
	$(call interpret_with_scope_and_compare,nv,B)
	$(call interpret_with_scope_and_compare,nv,C)
