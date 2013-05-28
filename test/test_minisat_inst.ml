(* Copyright (c) 2013 Radek Micek *)

module S = Ftest_anysat_inst.Make (Minisat_inst.Inst)

let suite = S.suite "Minisat_inst"
