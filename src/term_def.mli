(* Copyright (c) 2013 Radek Micek *)

(** Defining ground terms. *)

val define_ground_terms :
  Symb.db -> Clause.t BatDynArray.t -> Clause.t BatDynArray.t
