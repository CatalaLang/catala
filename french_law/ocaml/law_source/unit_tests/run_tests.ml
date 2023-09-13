let failure = ref false

let try_test msg test =
  try
    test ();
    Format.printf "@{<green>PASS@} @{<magenta>%s@}\n" msg
  with Runtime_ocaml.Runtime.AssertionFailed _ ->
    failure := true;
    Format.printf "@{<red>FAIL@} @{<magenta>%s@}\n" msg

let () =
  try_test "Allocations familiales #1" Tests_allocations_familiales.test1;
  try_test "Allocations familiales #2" Tests_allocations_familiales.test2;
  try_test "Allocations familiales #3" Tests_allocations_familiales.test3;
  try_test "Allocations familiales #4" Tests_allocations_familiales.test4;
  try_test "Allocations familiales #5" Tests_allocations_familiales.test5;
  try_test "Allocations familiales #6" Tests_allocations_familiales.test6;
  try_test "Allocations familiales #7" Tests_allocations_familiales.test7;
  try_test "Allocations familiales #8" Tests_allocations_familiales.test8;
  try_test "Allocations familiales #9" Tests_allocations_familiales.test9;
  try_test "Allocations familiales #10" Tests_allocations_familiales.test10;
  try_test "Allocations familiales #11" Tests_allocations_familiales.test11;
  try_test "Allocations familiales #12" Tests_allocations_familiales.test12;
  try_test "Allocations familiales #13" Tests_allocations_familiales.test13;
  try_test "Allocations familiales #14" Tests_allocations_familiales.test14;
  exit (if !failure then -1 else 0)
