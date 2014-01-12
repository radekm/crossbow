(* Copyright (c) 2013 Radek Micek *)

(** Models with single sort. *)

(** An interpretation of a symbol.

   The array [values] contains a value for each argument vector (point).
   In the case of a predicate symbol the value is [0] or [1].
   The order of the values in the array [values] is given by
   the lexical order of the argument vectors.
*)
type table = {
  values : (int, [`R]) Earray.t;
}

type t = {
  max_size : int;
  symbs : table Symb.Map.t;
}

(** [of_ms_model ms_model sorts] converts the multi-sorted model [ms_model]
   to a model with a single sort with [ms_model.Ms_model.max_size] elements.

   Sorts with an adequate size lower than [max_size] are extended
   ([ms_model] must interpret all constants from these sorts).
 *)
val of_ms_model : Ms_model.t -> Sorts.t -> t

val equal : t -> t -> bool

val compare : t -> t -> int

val canonize : t -> t

(** Constructs a set of non-isomorphic models with a single sort
   from the given multi-sorted model by fixing the domain of one sort and
   permuting the domains of the remaining sorts.

   Note: All domain sizes in the multi-sorted model must be equal
   to [max_size].
*)
val all_of_ms_model : Ms_model.t -> Sorts.t -> t BatSet.t
