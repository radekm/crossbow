(* Copyright (c) 2013 Radek Micek *)

(** Splitting. *)

(** Splitting strategy. *)
type t

(** Two variables are {b connected} iff there is a literal where
   they both occur. This relation is reflexive and symmetric.
*)

(** Repeatedly performs proper binary splits as follows:

   Finds the variable [x] which is connected to the least number
   of variables. Literals with [x] are placed to one clause
   and remaining literals are placed to the other clause.
*)
val paradox_splitting : t

(** Splits the given clause using the given strategy.

   This may introduce new auxiliary predicate symbols
   and generate new clause ids.
*)
val split_clause : t -> Prob.t -> Clause.t -> Clause.t list
