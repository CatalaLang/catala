#! /usr/bin/env bash

cd "$(dirname "$0")"
virtualenv -p python3 env
source env/bin/activate
make dependencies
