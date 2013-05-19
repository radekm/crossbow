(* Copyright (c) 2013 Radek Micek *)

(** Models with single sort. *)

type table = {
  values : int array;
}

type t = {
  max_size : int;
  symbs : (Symb.id, table) Hashtbl.t;
}

(** [of_ms_model ms_model sorts] converts the multi-sorted model [ms_model]
   to a model with a single sort with [ms_model.Ms_model.max_size] elements.

   Sorts with an adequate size lower than [max_size] are extended
   ([ms_model] must interpret all constants from these sorts).
 *)
val of_ms_model : Ms_model.t -> Sorts.t -> t
