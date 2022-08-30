#! /usr/bin/env sh

set -eu

cd "$(dirname "$0")"
cd pygments && python3 setup.py develop --user
