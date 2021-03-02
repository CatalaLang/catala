#! /usr/bin/env bash

cd "$(dirname "$0")"
cd pygments && python3 setup.py develop
