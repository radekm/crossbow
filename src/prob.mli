(* Copyright (c) 2013 Radek Micek *)

(** Representation of a problem. *)

type t = {
  clauses : Clause2.t BatDynArray.t;
  distinct_consts : Symb.id BatDynArray.t;
  symbols : Symb.db;
  next_clause_id : Clause2.id ref;
}

(** Creates an empty problem. *)
val create : unit -> t

(** Returns a new clause id. *)
val fresh_id : t -> Clause2.id
