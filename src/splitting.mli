(* Copyright (c) 2013 Radek Micek *)

(** Splitting. *)

(** Splitting strategy. *)
type 's t

(** Two variables are {b connected} iff there is a literal where
   they both occur. This relation is reflexive and symmetric.
*)

(** Repeatedly performs proper binary splits as follows:

   Finds the variable [x] which is connected to the least number
   of variables. Literals with [x] are placed to one clause
   and remaining literals are placed to the other clause.
*)
val paradox_splitting : 's t

(** Removes the ground literals and repeatedly performs
   proper binary splits as follows:

   Finds the variable [x] which is connected to the least number
   of variables. Let [lvars] be the set of all variables from the
   literals with [x]. Literals which contain only variables from [lvars]
   are placed to one clause and remaining literals are placed to
   the other clause.

   After the splitting is done the ground literals are added
   to a clause which has the (lexicographically)
   smallest (number of variables, number of literals).
*)
val paradox_mod_splitting : 's t

(** Splits the given clause using the given strategy.

   This may introduce new auxiliary predicate symbols
   and generate new clause ids.
*)
val split_clause : 's t -> 's Prob.t -> 's Clause.t -> 's Clause.t list
