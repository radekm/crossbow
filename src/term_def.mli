(* Copyright (c) 2013 Radek Micek *)

(** Defining ground terms. *)

val define_ground_terms :
  's Symb.db -> 's Clause.t BatDynArray.t -> 's Clause.t BatDynArray.t
