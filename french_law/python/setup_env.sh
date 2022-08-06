#! /usr/bin/env sh

set -eu

cd "$(dirname "$0")"
python3 -m venv env
. env/bin/activate
make dependencies
