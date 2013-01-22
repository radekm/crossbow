(* Copyright (c) 2013 Radek Micek *)

(** Additional functions for arrays.  *)

(** [pick f a] successively applies [f] to the elements of [a] until [f]
   succeeds. The result of [f] is returned if [f] succeeded, [None] otherwise.
*)
val pick : ('a -> 'b option) -> 'a array -> 'b option

(** [existsi p arr] checks if at least one element of
   the array satisfies the predicate [p]. [p] takes an element and its index.
*)
val existsi : (int -> 'a -> bool) -> 'a array -> bool

(** [rindex_of p a start len] returns the index of the rightmost element
   among [a.(start), .., a.(start+len-1)] which satisfies [p].
   [p] takes an element and its index.
*)
val rindex_of : (int -> 'a -> bool) -> 'a array -> int -> int -> int option

(** [iter_combinations f k arr] applies [f] to each [k]-combination
   of the elements of [arr]. The array in which the [k]-combinations
   are passed to [f] is the same for all calls to [f].
*)
val iter_combinations : ('a array -> unit) -> int -> 'a array -> unit
