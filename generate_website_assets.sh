#! /usr/bin/env sh

set -eux

cd "$(dirname "$0")"

if [ $# -ne 1 ]; then
  echo "USAGE: \$1 DST where DST is the directory in which files have to be copied"
  exit 1
fi

mkdir -p $1/playground

BUILD=_build/default

rsync -a $BUILD/_doc/_html/ $1/ocaml_docs/ --delete

rsync $BUILD/examples/allocations_familiales/allocations_familiales.html $1/
rsync $BUILD/examples/aides_logement/aides_logement.html $1/
rsync $BUILD/examples/us_tax_code/us_tax_code.html $1/
rsync $BUILD/examples/tutorial_en/tutorial_en.html $1/
rsync $BUILD/examples/tutoriel_fr/tutoriel_fr.html $1/

rsync $BUILD/grammar.html $1/
rsync $BUILD/catala.html $1/
rsync $BUILD/clerk.html $1/

rsync $BUILD/examples/allocations_familiales/allocations_familiales_schema.json $1/
rsync $BUILD/examples/aides_logement/aides_logement_schema.json $1/
