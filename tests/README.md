# Catala test suite

This folder contains Catala source files designed to test the features of the language.


It uses `make` to launch tests and compare the test terminal output with an expected output.

When you create a new test, please register it in the `Makefile` following the other examples. Expected outputs are stored using the convention `<name_of_test>.catala.<name_of_scope>.out` in the corresponding test folder.
