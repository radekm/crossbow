(* Copyright (c) 2013 Radek Micek *)

module S = Ftest_anycsp_inst.Make (Gecode_inst.Inst)

let suite = S.suite "Gecode_inst"
