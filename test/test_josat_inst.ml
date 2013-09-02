(* Copyright (c) 2013 Radek Micek *)

module S = Ftest_anysat_inst.Make (Josat_inst.Inst)

let suite = S.suite "Josat_inst"
