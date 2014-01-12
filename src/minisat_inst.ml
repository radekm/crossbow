(* Copyright (c) 2013 Radek Micek *)

module Minisat_ex : Sat_inst.Solver = struct
  include Minisat

  let new_false_var = Minisat.new_var

  let add_symmetry_clause = Minisat.add_clause

  let add_at_least_one_val_clause = Minisat.add_clause

  let add_at_most_one_val_clause s lits = Minisat.add_clause s lits 2

  let remove_clauses_with_lit s lit =
    ignore (Minisat.add_clause s (Earray.singleton lit) 1)
end

module Inst = Sat_inst.Make (Minisat_ex)
