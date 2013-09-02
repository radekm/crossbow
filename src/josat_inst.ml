(* Copyright (c) 2013 Radek Micek *)

module Josat_ex : Sat_inst.Solver = struct
  include Josat

  external new_false_var : t -> var = "josat_new_false_var"

  external add_symmetry_clause : t -> lit array -> int -> bool =
    "josat_add_single_value_constraint"

  external add_at_least_one_val_clause : t -> lit array -> int -> bool =
    "josat_add_single_value_constraint"

  let add_at_most_one_val_clause _ _ = true

  external force_simplify : t -> unit =
    "josat_force_simplify"

  let remove_clauses_with_lit s lit =
    ignore (Josat.add_clause s [| lit |] 1);
    (* Remove satisfied clauses.

       It is necessary to remove old single value constraints
       since each value variable can occur in at most one such constraint.
    *)
    force_simplify s

end

module Inst = Sat_inst.Make (Josat_ex)
