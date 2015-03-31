(* Copyright (c) 2015 Radek Micek *)

open OUnit2

let suite =
  "suite" >:::
    [
      Test_report.suite;
      Test_sign_test.suite;
    ]

let () = run_test_tt_main suite
