(* Copyright (c) 2013 Radek Micek *)

module S = Ftest_anysat.Make (Josat)

let suite = S.suite "Josat"
