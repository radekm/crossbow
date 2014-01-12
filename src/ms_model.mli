(* Copyright (c) 2013 Radek Micek *)

(** Multi-sorted models. *)

(** An interpretation of a symbol.

   The array [values] contains a value for each argument vector (point).
   In the case of a predicate symbol the value is [0] or [1].
   The order of the values in the array [values] is given by
   the lexical order of the argument vectors.
*)
type table = {
  param_sizes : (int, [`R]) Earray.t;
  (** Domain sizes of parameters. *)
  values : (int, [`R]) Earray.t;
}

type t = {
  max_size : int;
  symbs : table Symb.Map.t;
}

val equal : t -> t -> bool

val compare : t -> t -> int

val canonize : t -> Sorts.t -> t
