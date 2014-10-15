(* Copyright (c) 2013 Radek Micek *)

(** Representation of a problem. *)

type 'a t = {
  clauses : Clause2.t BatDynArray.t;
  symbols : 'a Symb.db;
  next_clause_id : Clause2.id ref;
}
constraint 'a = [< `R|`W]

(** Creates an empty problem. *)
val create : unit -> _ t

(** Drop to read-only permissions. *)
external read_only : [> `R] t -> [`R] t = "%identity"

(** Drop to write-only permissions. *)
external write_only : [> `W] t -> [`W] t = "%identity"

(** Returns a new clause id. *)
val fresh_id : [> `W] t -> Clause2.id
