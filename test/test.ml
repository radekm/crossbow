(* Copyright (c) 2013 Radek Micek *)

open OUnit

let suite =
  TestList
    [
      Test_earray.suite;
      Test_elist.suite;
      Test_algo.suite;
      Test_equiv.suite;
      Test_timer.suite;
      Test_symb.suite;
      Test_term.suite;
      Test_lit.suite;
      Test_clause.suite;
      Test_prop_det.suite;
      Test_term_def.suite;
      Test_splitting.suite;
      Test_tptp_prob.suite;
      Test_sorts.suite;
      Test_assignment.suite;
      Test_minisat.suite;
      Test_cmsat.suite;
      Test_josat.suite;
      Test_gecode.suite;
      Test_bliss.suite;
      Test_symred.suite;
      Test_lnh.suite;
      Test_ms_model.suite;
      Test_model.suite;
      Test_sat_inst.suite;
      Test_minisat_inst.suite;
      Test_cmsat_inst.suite;
      Test_josat_inst.suite;
      Test_csp_inst.suite;
      Test_gecode_inst.suite;
    ]

let () = ignore (run_test_tt suite)
