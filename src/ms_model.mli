(* Copyright (c) 2013 Radek Micek *)

(** Multi-sorted models. *)

(** An interpretation of a symbol.

   The array [values] contains a value for each argument vector (point).
   In the case of a predicate symbol the value is [0] or [1].
   The order of the values in the array [values] is given by
   the lexical order of the argument vectors.
*)
type table = {
  param_sizes : int array;
  (** Domain sizes of parameters. *)
  values : int array;
}

type 's t = {
  max_size : int;
  symbs : ('s, table) Symb.Map.t;
}

val equal : 's t -> 's t -> bool

val canonize : 's t -> 's Sorts.t -> 's t
