#! /bin/bash

JAVA_FILES=$(find src/dates_calc/ -name "*.java")

javac -d classes $JAVA_FILES

jar --main-class dates_calc/DatesCalcTest --create --file test-main.jar -C classes dates_calc
