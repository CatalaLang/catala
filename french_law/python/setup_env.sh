#! /usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"
python3 -m venv env
source env/bin/activate
make dependencies
