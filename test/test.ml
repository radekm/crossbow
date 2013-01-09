(* Copyright (c) 2013 Radek Micek *)

open OUnit

let suite =
  TestList
    [
      Test_earray.suite;
      Test_elist.suite;
      Test_symb.suite;
      Test_term.suite;
      Test_clause.suite;
    ]

let () = ignore (run_test_tt suite)