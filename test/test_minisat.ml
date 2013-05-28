(* Copyright (c) 2013 Radek Micek *)

module S = Ftest_anysat.Make (Minisat)

let suite = S.suite "Minisat"
