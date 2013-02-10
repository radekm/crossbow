(* Copyright (c) 2013 Radek Micek *)

open OUnit

let suite =
  TestList
    [
      Test_earray.suite;
      Test_elist.suite;
      Test_algo.suite;
      Test_equiv.suite;
      Test_symb.suite;
      Test_term.suite;
      Test_clause.suite;
      Test_tptp_prob.suite;
      Test_sorts.suite;
      Test_assignment.suite;
      Test_sinterp.suite;
      Test_minisat.suite;
      Test_symred.suite;
    ]

let () = ignore (run_test_tt suite)
