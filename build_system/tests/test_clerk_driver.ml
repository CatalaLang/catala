module D = Clerk_driver
module Al = Alcotest
module Nj = Ninja_utils

module To_test = struct
  let ninja_start = D.ninja_start

  let add_test_builds = D.add_test_builds
end

(* cwd: _build/default/build_system/tests/ *)
let test_files_dir = "../../../../build_system/tests/catala_files/"

let ninja_start = To_test.ninja_start "catala" ""

let al_assert msg = Al.(check bool) msg true

let test_ninja_start () =
  Buffer.clear Format.stdbuf;
  Nj.format Format.str_formatter ninja_start;
  let expected_format =
    "rule reset_with_scope\n  command = catala  -s $scope $catala_cmd $tested_file $extra_flags --unstyled > $expected_output 2>&1\n  description = RESET scope $scope of file $tested_file with the $catala_cmd command\n\nrule reset_with_scope_and_output\n  command = catala  -s $scope $catala_cmd $tested_file $extra_flags --unstyled -o $expected_output 2>&1\n  description = RESET scope $scope of file $tested_file with the $catala_cmd command\n\nrule reset_without_scope\n  command = catala  $catala_cmd $tested_file $extra_flags --unstyled > $expected_output 2>&1\n  description = RESET file $tested_file with the $catala_cmd command\n\nrule reset_without_scope_and_output\n  command = catala  $catala_cmd $tested_file $extra_flags --unstyled -o $expected_output 2>&1\n  description = RESET file $tested_file with the $catala_cmd command\n\nrule run_and_display_final_message\n  command = :\n  description = All tests $test_file_or_folder passed!\n\nrule test_with_scope\n  command = catala  -s $scope $catala_cmd $tested_file $extra_flags --unstyled 2>&1 | colordiff -u -b $expected_output -\n  description = TEST scope $scope of file $tested_file with the $catala_cmd command\n\nrule test_with_scope_and_output\n  command = catala  -s $scope $catala_cmd $tested_file $extra_flags --unstyled -o $tmp_file ; colordiff -u -b $expected_output $tmp_file\n  description = TEST scope $scope of file $tested_file with the $catala_cmd command\n\nrule test_without_scope\n  command = catala  $catala_cmd $tested_file $extra_flags --unstyled 2>&1 | colordiff -u -b $expected_output -\n  description = TEST on file $tested_file with the $catala_cmd command\n\nrule test_without_scope_and_output\n  command = catala  $catala_cmd $tested_file $extra_flags --unstyled -o $tmp_file ; colordiff -u -b $expected_output $tmp_file\n  description = TEST on file $tested_file with the $catala_cmd command\n\n"[@ocamlformat "disable"]
  in
  let actual_format = Buffer.contents Format.stdbuf in
  Al.(check string) "both formated strings should equal" expected_format actual_format

let test_add_test_builds_for_folder () =
  let ctx = D.ninja_building_context_init ninja_start in
  let nj_building_ctx = To_test.add_test_builds ctx [ test_files_dir ^ "folder" ] false in
  al_assert "a test case should be found" (Option.is_some nj_building_ctx.curr_ninja);

  let expected_format =
    "build test_A_Interpret_..-..-..-..-build_system-tests-catala_files-folder-file1.catala_en: test_with_scope\n  scope = A\n  catala_cmd = Interpret\n  tested_file = ../../../../build_system/tests/catala_files/folder/file1.catala_en\n  expected_output = ../../../../build_system/tests/catala_files/folder/output/file1.catala_en.A.Interpret\nbuild test_B_Interpret_..-..-..-..-build_system-tests-catala_files-folder-file1.catala_en: test_with_scope\n  scope = B\n  catala_cmd = Interpret\n  tested_file = ../../../../build_system/tests/catala_files/folder/file1.catala_en\n  expected_output = ../../../../build_system/tests/catala_files/folder/output/file1.catala_en.B.Interpret\nbuild test_Proof_..-..-..-..-build_system-tests-catala_files-folder-file3.catala_en: test_without_scope\n  extra_flags = --disable_counterexamples\n  catala_cmd = Proof\n  tested_file = ../../../../build_system/tests/catala_files/folder/file3.catala_en\n  expected_output = ../../../../build_system/tests/catala_files/folder/output/file3.catala_en.Proof\nbuild test_Typecheck_..-..-..-..-build_system-tests-catala_files-folder-file2.catala_en: test_without_scope\n  catala_cmd = Typecheck\n  tested_file = ../../../../build_system/tests/catala_files/folder/file2.catala_en\n  expected_output = ../../../../build_system/tests/catala_files/folder/output/file2.catala_en.Typecheck\nbuild test_dir_..-..-..-..-build_system-tests-catala_files-folder: run_and_display_final_message  $\n  test_file_..-..-..-..-build_system-tests-catala_files-folder-file3.catala_en $\n  test_file_..-..-..-..-build_system-tests-catala_files-folder-file2.catala_en $\n  test_file_..-..-..-..-build_system-tests-catala_files-folder-file1.catala_en\n  test_file_or_folder = in folder '../../../../build_system/tests/catala_files/folder'\nbuild test_file_..-..-..-..-build_system-tests-catala_files-folder-file1.catala_en: phony  $\n  test_A_Interpret_..-..-..-..-build_system-tests-catala_files-folder-file1.catala_en $\n  test_B_Interpret_..-..-..-..-build_system-tests-catala_files-folder-file1.catala_en\nbuild test_file_..-..-..-..-build_system-tests-catala_files-folder-file2.catala_en: phony  $\n  test_Typecheck_..-..-..-..-build_system-tests-catala_files-folder-file2.catala_en\nbuild test_file_..-..-..-..-build_system-tests-catala_files-folder-file3.catala_en: phony  $\n  test_Proof_..-..-..-..-build_system-tests-catala_files-folder-file3.catala_en\n"[@ocamlformat "disable"]
  in
  let actual_format =
    let ninja = Option.get nj_building_ctx.curr_ninja in
    Buffer.clear Format.stdbuf;
    Nj.BuildMap.iter (fun _ b -> Nj.Build.format Format.str_formatter b) ninja.builds;
    Buffer.contents Format.stdbuf
  in
  Al.(check string) "both formated strings should equal" expected_format actual_format

let test_add_test_builds_for_untested_file () =
  let untested_file = test_files_dir ^ "untested_file.catala_en" in
  let ctx = D.ninja_building_context_init Nj.empty in
  let nj_building_ctx = To_test.add_test_builds ctx [ untested_file ] false in

  al_assert "no test cases should be found" (Option.is_none nj_building_ctx.curr_ninja);
  al_assert "ninja_start should be the last valid ninja"
    (Nj.empty = nj_building_ctx.last_valid_ninja)

(* Test without comparing formated ninja. *)
let test_add_test_builds_for_simple_interpret_scope_file () =
  let simple_interpret_scope_file = test_files_dir ^ "simple_interpret_scope_file.catala_en" in
  let ctx = D.ninja_building_context_init ninja_start in
  let nj_building_ctx = To_test.add_test_builds ctx [ simple_interpret_scope_file ] false in
  al_assert "a test case should be found" (Option.is_some nj_building_ctx.curr_ninja);

  let expected_format =
    let open Nj in
    let test_file_output = "test_file_" ^ Nj.Build.unpath simple_interpret_scope_file in
    let test_A_file_output = "test_A_Interpret_" ^ Nj.Build.unpath simple_interpret_scope_file in
    let test_A_file =
      Build.make_with_vars ~outputs:[ Expr.Lit test_A_file_output ] ~rule:"test_with_scope"
        ~vars:
          [
            ("scope", Lit "A");
            ("catala_cmd", Lit "Interpret");
            ("tested_file", Lit simple_interpret_scope_file);
            ( "expected_output",
              Lit (test_files_dir ^ "output/simple_interpret_scope_file.catala_en.A.Interpret") );
          ]
    in
    let test_file =
      Build.make_with_inputs ~outputs:[ Expr.Lit test_file_output ] ~rule:"phony"
        ~inputs:[ Expr.Lit (" $\n  " ^ test_A_file_output) ]
    in
    BuildMap.empty
    |> BuildMap.add test_file_output test_file
    |> BuildMap.add test_A_file_output test_A_file
    |> Nj.BuildMap.iter (fun _ b -> Nj.Build.format Format.str_formatter b);
    Buffer.contents Format.stdbuf
  in

  let actual_format =
    let ninja = Option.get nj_building_ctx.curr_ninja in
    Buffer.clear Format.stdbuf;
    Nj.BuildMap.iter (fun _ b -> Nj.Build.format Format.str_formatter b) ninja.builds;
    Buffer.contents Format.stdbuf
  in

  Al.(check string) "both formated strings should equal" expected_format actual_format

let () =
  Al.run "Clerk_driver"
    Al.
      [
        ( "Test ninja_start",
          [ test_case "initial ninja rules should be present" `Quick test_ninja_start ] );
        ( "Test add_test_builds",
          [
            test_case "an untested file" `Quick test_add_test_builds_for_untested_file;
            test_case "a simple Interpret scope" `Quick
              test_add_test_builds_for_simple_interpret_scope_file;
            test_case "a simple folder" `Quick test_add_test_builds_for_folder;
          ] );
      ]
