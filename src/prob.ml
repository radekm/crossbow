(* Copyright (c) 2013 Radek Micek *)

type 'a t = {
  clauses : Clause2.t BatDynArray.t;
  distinct_consts : Symb.id BatDynArray.t;
  symbols : 'a Symb.db;
  next_clause_id : Clause2.id ref;
}
constraint 'a = [< `R|`W]

let create () =
  let symbols = Symb.create_db () in
  {
    clauses = BatDynArray.create ();
    distinct_consts = BatDynArray.create ();
    symbols;
    next_clause_id = ref 0;
  }

external read_only : [> `R] t -> [`R] t = "%identity"

external write_only : [> `W] t -> [`W] t = "%identity"

let fresh_id prob =
  let id = !(prob.next_clause_id) in
  incr prob.next_clause_id;
  id
