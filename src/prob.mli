(* Copyright (c) 2013 Radek Micek *)

(** Representation of a problem. *)

type 's t = {
  clauses : 's Clause2.t BatDynArray.t;
  distinct_consts : 's Symb.id BatDynArray.t;
  symbols : 's Symb.db;
  next_clause_id : Clause2.id ref;
}

type wt =
  | Wr : 's t -> wt

(** Creates an empty problem. *)
val create : unit -> wt

(** Returns a new clause id. *)
val fresh_id : 's t -> Clause2.id
