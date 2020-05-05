#! /usr/bin/env bash

cd "$(dirname "$0")"

if [[ $1 == "" ]]; then
  echo "USAGE: $1 DST where DST is the directory in which files have to be copied"
  exit 1
fi

dest_dir=$1

make -C examples/allocations_familiales allocations_familiales.html
make grammar.html
make catala.html
make legifrance_catala.html

cp  examples/allocations_familiales/allocations_familiales.html $1/
cp grammar.html $1/
cp catala.html $1/
cp legifrance_catala.html $1/
