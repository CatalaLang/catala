#! /bin/bash

set -e

TESTS_DIRS=$(find tests -name "good")
CATALA_FILES=${1:-$(find $TESTS_DIRS -name "*.catala_en")}

OUT_DIR=/tmp/java_gen

for i in $CATALA_FILES; do
    TEST_DIR=$(dirname $i | xargs dirname | xargs basename)
    echo "Compiling $i"
    catala java $i -O -o $OUT_DIR/$TEST_DIR
done

TESTS_DIRS=""
for i in $CATALA_FILES; do
    TESTS_DIRS+="$OUT_DIR/$(dirname $i | xargs dirname | xargs basename) "
done

TESTS_DIRS=$(echo $TESTS_DIRS | tr " " "\n" | sort | uniq)

for TEST_DIR in $TESTS_DIRS; do
    echo "Running maven on $TEST_DIR"
    pushd $TEST_DIR > /dev/null
    if !(mvn compile > /tmp/log); then cat /tmp/log; exit; fi
    popd > /dev/null
done
