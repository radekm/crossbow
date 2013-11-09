(* Copyright (c) 2013 Radek Micek *)

(** Symmetry reduction. *)

type args = int array

type cell = Symb.id * args

(** Contains data for symmetry reduction. *)
type t

(** Note: The problem and the sort information must not be modified after
   this call.
*)
val create : Prob.t -> Sorts.t -> t

(** Assigns distinct constants and restricts range of some cells.
   Returns a list of pairs [(cell, (lo, hi))] where [lo] and [hi]
   are inclusive bounds for the value of [cell].
*)
val incr_max_size : t -> (cell * (int * int)) list
