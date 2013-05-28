(* Copyright (c) 2013 Radek Micek *)

module S = Ftest_anysat.Make (Cmsat)

let suite = S.suite "Cmsat"
