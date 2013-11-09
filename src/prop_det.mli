(* Copyright (c) 2013 Radek Micek *)

(** Property detector. *)

(** Detect and remove commutativity axioms from clauses.
   Information about commutativity is saved to symbol database.
*)
val detect_commutativity :
  Symb.db -> Clause.t BatDynArray.t -> Clause.t BatDynArray.t

(** Detect group axioms and add hints [Symb.Latin_square] to multiplication
   symbols and [Symb.Permutation] to inversion symbols.
*)
val detect_hints_for_groups : Symb.db -> Clause.t BatDynArray.t -> unit

(** Detect quasigroup axioms and add hints [Symb.Latin_square]
   to multiplication symbols.
*)
val detect_hints_for_quasigroups :
  Symb.db -> Clause.t BatDynArray.t -> unit

(** For each involutivity axiom [f(f(x)) = x] add hint [Symb.Permutation]
   to symbol [f].
*)
val detect_hints_for_involutive_funcs :
  Symb.db -> Clause.t BatDynArray.t -> unit
