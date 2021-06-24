#! /usr/bin/env bash

virtualenv -p python3 env
source env/bin/activate
make dependencies
