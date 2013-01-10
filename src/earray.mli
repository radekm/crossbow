(* Copyright (c) 2013 Radek Micek *)

(** Additional functions for arrays.  *)

(** [pick f a] successively applies [f] to the elements of [a] until [f]
   succeeds. The result of [f] is returned if [f] succeeded, [None] otherwise.
*)
val pick : ('a -> 'b option) -> 'a array -> 'b option

(** [iter_combinations f k arr] applies [f] to each [k]-combination
   of the elements of [arr]. The array in which the [k]-combinations
   are passed to [f] is the same for all calls to [f].
*)
val iter_combinations : ('a array -> unit) -> int -> 'a array -> unit
