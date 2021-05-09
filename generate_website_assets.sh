#! /usr/bin/env bash

cd "$(dirname "$0")"

if [[ $1 == "" ]]; then
  echo "USAGE: $1 DST where DST is the directory in which files have to be copied"
  exit 1
fi

rsync -a _build/default/_doc/_html/ $1/ocaml_docs/
scp examples/allocations_familiales/allocations_familiales.html $1/
scp examples/us_tax_code/us_tax_code.html $1/
scp examples/tutorial_en/tutorial_en.html $1/
scp examples/tutoriel_fr/tutoriel_fr.html $1/
scp grammar.html $1/
scp catala.html $1/
scp _build/default/src/catala/catala_web.bc.js $1/playground/
scp examples/tutorial_en/tutorial_en.catala_en $1/playground/
# TODO BW: PL
scp examples/tutoriel_fr/tutoriel_fr.catala_fr $1/playground/
cat examples/allocations_familiales/prologue.catala_fr > allocations_familiales.catala_fr
cat examples/allocations_familiales/decrets_divers.catala_fr >> allocations_familiales.catala_fr
cat examples/allocations_familiales/securite_sociale_L.catala_fr >> allocations_familiales.catala_fr
cat examples/allocations_familiales/securite_sociale_R.catala_fr >> allocations_familiales.catala_fr
cat examples/allocations_familiales/securite_sociale_D.catala_fr >> allocations_familiales.catala_fr
cat examples/allocations_familiales/epilogue.catala_fr >> allocations_familiales.catala_fr
cat examples/allocations_familiales/tests/tests_allocations_familiales.catala_fr >> allocations_familiales.catala_fr
sed -r '/^## Inclusion.+$/d' allocations_familiales.catala_fr > allocations_familiales_fixed.catala_fr
mv -f allocations_familiales_fixed.catala_fr allocations_familiales.catala_fr
scp allocations_familiales.catala_fr $1/playground/
rm allocations_familiales.catala_fr
scp syntax_highlighting/en/ace/mode-catala_en.js $1/playground/
# TODO BW: PL
scp syntax_highlighting/fr/ace/mode-catala_fr.js $1/playground/
scp french_law_js/french_law.js $1/french_law.js
