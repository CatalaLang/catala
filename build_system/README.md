# Clerk

The build system for Catala.

## Build

* to build: `dune build`
* to test: `dune test`
* to run: `dune exec ./clerk.exe`

## Usage

### To test a specific file at `path/to/file.catala_en`

The command `clerk test path/to/file.catala_en` will run tests
for each tested scope of `path/to/file.catala_en`.

## Mechanics

`build.ninja` architecture:

```ninja
rule test_scope
  command = catala -s $scope Interpret $tested_file --unstyle | colordiff -u -b $expected_output -
  description = Testing scope $scope of file $tested_file

build test: phony $
  test_<path-to-tested-dir1> $
  test_<path-to-tested-dir2>

build test_<path-to-tested-dir1>: phony $
  test_<path-to-tested-dir1-file1.catala_en> $
  test_<path-to-tested-dir1-file2.catala_en>

build test_<path-to-tested-dir1-file1.catala_en: phony $
  test_<scope1>_<path-to-tested-dir1-file1.catala_en.catala_en> $
  test_<scope2>_<path-to-tested-dir1-file1.catala_en.catala_en>

build test_<scope1>_<path-to-tested-dir1-file1.catala_en.catala_en>: test_scope
  scope = <scope1>
  tested_file = <path/to/tested/dir1/file1.catala_en>
  expected_output = <path/to/tested/dir1/output/file1.catala_en.<scope1>.Interpret

build test_<scope2>_<path-to-tested-dir1-file1.catala_en.catala_en: test_scope
  scope = <scope2>
  tested_file = <path/to/tested/dir1/file1.catala_en>
  expected_output = <path/to/tested/dir1/output/file1.catala_en.<scope2>.Interpret

...
```
