let failure = ref false

let try_test msg test =
  try
    test ();
    Format.printf "%s %s\n"
      (ANSITerminal.sprintf [ ANSITerminal.green ] "PASS")
      (ANSITerminal.sprintf [ ANSITerminal.magenta ] msg)
  with Runtime.AssertionFailed ->
    failure := true;
    Format.printf "%s %s\n"
      (ANSITerminal.sprintf [ ANSITerminal.red ] "FAIL")
      (ANSITerminal.sprintf [ ANSITerminal.magenta ] msg)

let _ =
  try_test "Allocations familiales #1" Tests_allocations_familiales.test1;
  try_test "Allocations familiales #2" Tests_allocations_familiales.test2;
  try_test "Allocations familiales #3" Tests_allocations_familiales.test3;
  try_test "Allocations familiales #4" Tests_allocations_familiales.test4;
  try_test "Allocations familiales #5" Tests_allocations_familiales.test5;
  try_test "Allocations familiales #6" Tests_allocations_familiales.test6;
  exit (if !failure then -1 else 0)
