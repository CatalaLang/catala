open Alcotest
open Ninja_utils
module B = Build
module E = Expr

module To_test = struct
  let build_to_string = B.to_string
end

let test_empty_build () =
  check string "Must be the same string" "build empty: phony\n" (To_test.build_to_string B.empty)

let test_empty_build_w_inputs () =
  check string "Must be the same string" "build output: rule in1 in2\n"
    (To_test.build_to_string
       (B.make_with_inputs ~outputs:[ E.Lit "output" ] ~rule:"rule"
          ~inputs:[ E.Lit "in1"; E.Lit "in2" ]))

let () =
  run "Ninja builds"
    [
      ( "to_string",
        [
          test_case "Empty build" `Quick test_empty_build;
          test_case "Empty build \\w inputs" `Quick test_empty_build_w_inputs;
        ] );
    ]
