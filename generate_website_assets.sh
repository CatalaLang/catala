#! /usr/bin/env bash

cd "$(dirname "$0")"

if [[ $1 == "" ]]; then
  echo "USAGE: $1 DST where DST is the directory in which files have to be copied"
  exit 1
fi

make website-assets

scp examples/allocations_familiales/allocations_familiales.html $1/
scp examples/dummy_english/english.html $1/
scp grammar.html $1/
scp catala.html $1/
scp legifrance_catala.html $1/
