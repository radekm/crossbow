(* Copyright (c) 2013 Radek Micek *)

module Cmsat_ex : Sat_inst.Solver = struct
  include Cmsat

  let new_false_var = Cmsat.new_var

  let add_symmetry_clause = Cmsat.add_clause

  let add_at_least_one_val_clause = Cmsat.add_clause

  let add_at_most_one_val_clause s lits = Cmsat.add_clause s lits 2

  let remove_clauses_with_lit s lit =
    ignore (Cmsat.add_clause s [| lit |] 1)
end

module Inst = Sat_inst.Make (Cmsat_ex)
