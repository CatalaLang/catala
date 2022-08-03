#! /usr/bin/env sh

set -eu

cd "$(dirname "$0")"

if [ $# -eq 0 ]; then
  echo "USAGE: \$1 DST where DST is the directory in which files have to be copied"
  exit 1
fi

rsync -a _build/default/_doc/_html/ $1/ocaml_docs/
scp examples/allocations_familiales/allocations_familiales.html $1/
scp examples/aides_logement/aides_logement.html $1/
scp examples/us_tax_code/us_tax_code.html $1/
scp examples/tutorial_en/tutorial_en.html $1/
scp examples/tutoriel_fr/tutoriel_fr.html $1/
scp grammar.html $1/
scp catala.html $1/
scp french_law/js/french_law.js $1/french_law.js
scp examples/allocations_familiales/allocations_familiales_schema.json $1/
scp examples/aides_logement/aides_logement_schema.json $1/
