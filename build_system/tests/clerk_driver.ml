module To_test = struct
  let catala_backend_to_string = Clerk_driver.catala_backend_to_string
end

let test_catala_backend_to_string () =
  Alcotest.(check string) "same string" "Dcalc" (To_test.catala_backend_to_string Utils.Cli.Dcalc)

let () =
  let open Alcotest in
  run "Clerk"
    [ ("catala_backend_to_string", [ test_case "Dcalc" `Quick test_catala_backend_to_string ]) ]
