(* Copyright (c) 2013 Radek Micek *)

(** Property detector. *)

(** Detect and remove commutativity axioms from clauses.
   Information about commutativity is saved to symbol database.
*)
val detect_commutativity :
  's Symb.db -> 's Clause.t BatDynArray.t -> 's Clause.t BatDynArray.t

(** Detect group axioms and add hints [Symb.Latin_square] to multiplication
   symbols and [Symb.Permutation] to inversion symbols.
*)
val detect_hints_for_groups : 's Symb.db -> 's Clause.t BatDynArray.t -> unit

(** Detect quasigroup axioms and add hints [Symb.Latin_square]
   to multiplication symbols.
*)
val detect_hints_for_quasigroups :
  's Symb.db -> 's Clause.t BatDynArray.t -> unit
