(* Copyright (c) 2013 Radek Micek *)

module Josat_ex : Sat_inst.Solver = struct
  include Josat

  external new_false_var : t -> var = "josat_new_false_var"

  external add_symmetry_clause : t -> (lit, [> `R]) Earray.t -> int -> bool =
    "josat_add_single_value_constraint"

  external add_at_least_one_val_clause : t -> (lit, [> `R]) Earray.t -> int ->
    bool = "josat_add_single_value_constraint"

  let add_at_most_one_val_clause _ _ = true

  external remove_clauses_with_lit' : t -> lit -> unit =
    "josat_remove_clauses_with_lit"

  let remove_clauses_with_lit s lit =
    ignore (Josat.add_clause s (Earray.singleton lit) 1);
    (* It is necessary to remove old single value constraints
       since each value variable can occur in at most one such constraint.
    *)
    remove_clauses_with_lit' s lit

end

module Inst = Sat_inst.Make (Josat_ex)
