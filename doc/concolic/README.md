# Running the concolic engine 

You can run the concolic engine on a file F with scope S using `dune exec catala -- Concolic F -s S`

For example, running one of the tests can be done through `dune exec catala -- Concolic tests/test_concolic/good/except_one.catala_en  -s A`.

# Real-World Testcase 

## French Housing Benefits

```
git clone -b concolic git@github.com:rmonat/catala-examples.git
dune exec catala -- Concolic ./catala-examples/aides_logement/tests/concolic/simple_nochild_noassert.catala_fr -s Test
```

The file `simple_nochild_noassert` computes french housing benefits, with some restrictions, the main ones being that the resident is in mainland France, and without children.

The concolic engine will yield a complete set of testcases:
```
[RESULT] Evaluating with inputs:
[RESULT] . loyer_principal_in = 42,00 €
[RESULT] . ressources_ménage_arrondies_in = 42,00 €
[RESULT] . réduction_loyer_solidarité_in = 42,00 €
[RESULT] Output of scope after evaluation:
[RESULT] . aide_finale_formule = 62,17 €
[RESULT] 
[RESULT] Evaluating with inputs:
[RESULT] . loyer_principal_in = 0,00 €
[RESULT] . ressources_ménage_arrondies_in = 4 949,00 €
[RESULT] . réduction_loyer_solidarité_in = 0,00 €
[RESULT] Output of scope after evaluation:
[RESULT] . aide_finale_formule = 20,17 €
[RESULT] 
[RESULT] Evaluating with inputs:
[RESULT] . loyer_principal_in = 0,00 €
[RESULT] . ressources_ménage_arrondies_in = 5 661,90 €
[RESULT] . réduction_loyer_solidarité_in = 0,00 €
[RESULT] Output of scope after evaluation:
[RESULT] . aide_finale_formule = 0,00 €
[...]
```

You can use `--stats` to obtain some statistics on the concolic testing process:
```
=== Concolic execution statistics ===
General steps:
  create context: 0.012 s
  simplify: 0.038 s
  total loop time: 21.262 s
After 766 execution steps:
  print path constraints: 7.396 s
  extract solver constraints: 0.656 s
  solve: 6.965 s
  eval: 6.015 s
  choose new path constraints: 0.210 s
Total concolic time: 21.311 s
15 tests
======
```
You can also run a search for more testcase by reducing the constraints and enabling some optimizations:
```
dune exec catala -- Concolic --stats --optimize --conc-optim=trivial --conc-optim=lazy-default -s Test ../catala-examples/aides_logement/tests/concolic/large_metropole_apl.catala_fr
```

```
=== Concolic execution statistics ===
General steps:
  create context: 0.006 s
  simplify: 0.022 s
  total loop time: 3167.162 s
After 126159 execution steps:
  print path constraints: 0.032 s
  extract solver constraints: 0.990 s
  solve: 825.921 s
  eval: 2338.028 s
  choose new path constraints: 1.827 s
Total concolic time: 3167.190 s
17435 tests
======
```
