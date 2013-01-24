(* Copyright (c) 2013 Radek Micek *)

(** Interpretations of symbols. *)

(** An interpretation of a symbol.

   The array [values] contains a value for each argument vector (point).
   In the case of a predicate symbol the value is [0] or [1],
   in the case of a function symbol the value comes from the domain
   of its result. The order of the values in the array [values] is given by
   the lexical order of the argument vectors.

   The array [params] contains a domain size of each parameter.
*)
type t = {
  params : int array;
  values : int array;
}

(** [create arity adeq_sizes max_size] creates an interpretation
   containing zeros.
*)
val create : int -> int array -> int -> t

(** [get i args] returns the value in the point [args]. *)
val get : t -> int array -> int

(** [set i args v] sets the value [v] in the point [args]. *)
val set : t -> int array -> int -> unit
