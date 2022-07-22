#! /usr/bin/env sh

set -eu

cd "$(dirname "$0")"
python3 -m venv env
source env/bin/activate
make dependencies
