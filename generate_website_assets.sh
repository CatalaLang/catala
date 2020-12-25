#! /usr/bin/env bash

cd "$(dirname "$0")"

if [[ $1 == "" ]]; then
  echo "USAGE: $1 DST where DST is the directory in which files have to be copied"
  exit 1
fi

make website-assets

rsync -a _build/default/_doc/_html/ $1/ocaml_docs/
scp examples/allocations_familiales/allocations_familiales.html $1/
scp examples/us_tax_code/us_tax_code.html $1/
scp examples/tutorial_en/tutorial_en.html $1/
scp examples/tutoriel_fr/tutoriel_fr.html $1/
scp grammar.html $1/
scp catala.html $1/
scp _build/default/src/catala.bc.js $1/playground/
scp examples/tutorial_en/tutorial_en.catala_en $1/playground/
scp examples/tutoriel_fr/tutoriel_fr.catala_fr $1/playground/
scp syntax_highlighting/en/ace/mode-catala_en.js $1/playground/
scp syntax_highlighting/fr/ace/mode-catala_fr.js $1/playground/