(* Copyright (c) 2013 Radek Micek *)

(** Symmetry reduction. *)

type args = int array

type 's cell = 's Symb.id * args

(** Contains data for symmetry reduction. *)
type 's t

(** Note: The problem and the sort information must not be modified after
   this call.
*)
val create : 's Prob.t -> 's Sorts.t -> 's t

(** Assigns distinct constants and restricts range of some cells.
   Returns a list of pairs [(cell, (lo, hi))] where [lo] and [hi]
   are inclusive bounds for the value of [cell].
*)
val incr_max_size : 's t -> ('s cell * (int * int)) list
