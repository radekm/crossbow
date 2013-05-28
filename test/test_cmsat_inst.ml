(* Copyright (c) 2013 Radek Micek *)

module S = Ftest_anysat_inst.Make (Cmsat_inst.Inst)

let suite = S.suite "Cmsat_inst"
