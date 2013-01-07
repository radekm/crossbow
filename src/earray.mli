(* Copyright (c) 2013 Radek Micek *)

(** Additional functions for arrays.  *)

(** [pick f a] successively applies [f] to the elements of [a] until [f]
   succeeds. The result of [f] is returned if [f] succeeded, [None] otherwise.
*)
val pick : ('a -> 'b option) -> 'a array -> 'b option
