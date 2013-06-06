(* Copyright (c) 2013 Radek Micek *)

(** Models with single sort. *)

(** An interpretation of a symbol.

   The array [values] contains a value for each argument vector (point).
   In the case of a predicate symbol the value is [0] or [1].
   The order of the values in the array [values] is given by
   the lexical order of the argument vectors.
*)
type table = {
  values : int array;
}

type 's t = {
  max_size : int;
  symbs : ('s Symb.id, table) Hashtbl.t;
}

(** [of_ms_model ms_model sorts] converts the multi-sorted model [ms_model]
   to a model with a single sort with [ms_model.Ms_model.max_size] elements.

   Sorts with an adequate size lower than [max_size] are extended
   ([ms_model] must interpret all constants from these sorts).
 *)
val of_ms_model : 's Ms_model.t -> 's Sorts.t -> 's t
