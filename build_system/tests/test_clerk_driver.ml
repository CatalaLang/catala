module D = Clerk_driver
module Al = Alcotest
module Nj = Ninja_utils

module To_test = struct
  let collect_in_file = D.collect_in_file
end

(* cwd: _build/default/build_system/tests/ *)
let test_files_dir = "../../../../build_system/tests/catala_files/"

let ninja_start = D.ninja_start "catala" ""

let al_assert msg = Al.(check bool) msg true

let test_collect_in_untested_file () =
  let untested_file = test_files_dir ^ "untested_file.catala_en" in
  let ctx = D.ninja_building_context_init Nj.empty in
  let nj_building_ctx = To_test.collect_in_file ctx untested_file Nj.empty false in

  al_assert "no test cases should be found" (Option.is_none nj_building_ctx.curr_ninja);
  al_assert "ninja_start should be the last valid ninja"
    (Nj.empty = nj_building_ctx.last_valid_ninja)

let test_collect_in_simple_interpret_scope_file () =
  let simple_interpret_scope_file = test_files_dir ^ "simple_interpret_scope_file.catala_en" in
  let ctx = D.ninja_building_context_init ninja_start in
  let nj_building_ctx = To_test.collect_in_file ctx simple_interpret_scope_file ninja_start false in
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

  Al.(check string) "should be equal" expected_format actual_format

let suite_collect_in_file =
  Al.
    [
      test_case "an untested file" `Quick test_collect_in_untested_file;
      test_case "a simple Interpret scope" `Quick test_collect_in_simple_interpret_scope_file;
    ]

let () = Al.run "Clerk" [ ("Test collect_in_file", suite_collect_in_file) ]
