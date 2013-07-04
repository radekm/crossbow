(* Copyright (c) 2013 Radek Micek *)

(** Property detector. *)

(** Detect and remove commutativity axioms from clauses.
   Information about commutativity is saved to symbol database.
*)
val detect_commutativity :
  's Symb.db -> 's Clause.t BatDynArray.t -> 's Clause.t BatDynArray.t
