let _ =
  let open Alcotest in
  run "Optimizations"
    [
      ( "Lcalc",
        [
          test_case "#1" `Quick Lcalc.Optimizations.test_lcalc_optims1;
          test_case "#2" `Quick Lcalc.Optimizations.test_lcalc_optims2;
        ] );
    ]
