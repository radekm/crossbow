(* Copyright (c) 2013 Radek Micek *)

type 's t = {
  clauses : 's Clause.t BatDynArray.t;
  distinct_consts : 's Symb.id BatDynArray.t;
  symbols : 's Symb.db;
  next_clause_id : Clause.id ref;
}

type wt =
  | Wr : 's t -> wt

let create () =
  let Symb.Wr symbols = Symb.create_db () in
  Wr {
    clauses = BatDynArray.create ();
    distinct_consts = BatDynArray.create ();
    symbols;
    next_clause_id = ref 0;
  }

let fresh_id prob =
  let id = !(prob.next_clause_id) in
  incr prob.next_clause_id;
  id
