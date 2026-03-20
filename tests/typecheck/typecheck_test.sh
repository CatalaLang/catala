#!/bin/bash
# Regression test for `clerk typecheck` on non-module files.
#
# Bug: `clerk typecheck` on a file with no `> Module` declaration (e.g. a test
# file starting with `> Using ...`) would generate an empty ninja default
# target, causing ninja to fail with "expected target name" rather than
# actually typechecking the file.

set -e
cd "$(dirname "$0")"

${CLERK:-clerk} start
${CLERK:-clerk} typecheck my_test.catala_en
