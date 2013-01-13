(* Copyright (c) 2013 Radek Micek *)

type t = {
  clauses : Clause.t BatDynArray.t;
  distinct_consts : Symb.id BatDynArray.t;
  symbols : Symb.db;
  next_clause_id : Clause.id ref;
}

let create () = {
  clauses = BatDynArray.create ();
  distinct_consts = BatDynArray.create ();
  symbols = Symb.create_db ();
  next_clause_id = ref 0;
}

let fresh_id prob =
  let id = !(prob.next_clause_id) in
  incr prob.next_clause_id;
  id
