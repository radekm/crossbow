(* Copyright (c) 2013 Radek Micek *)

(** Additional functions for lists. *)

(** [contains x xs] checks whether the list [xs] contains the element [x]. *)
val contains : 'a -> 'a list -> bool

(** Same as {!List.exists}, but the predicate additionaly takes
   the index of the element.
*)
val existsi : (int -> 'a -> bool) -> 'a list -> bool

(** [pick f xs] successively applies [f] to the elements of [xs] until [f]
   succeeds. The result of [f] is returned if [f] succeeded, [None] otherwise.
*)
val pick : ('a -> 'b option) -> 'a list -> 'b option

(** Same as {!pick}, but the function additionaly takes
   the index of the element.
*)
val picki : (int -> 'a -> 'b option) -> 'a list -> 'b option

(** Same as {!pick}, but additionaly returns the input list without the
   element where the function succeeded.
*)
val pick_and_remove : ('a -> 'b option) -> 'a list -> 'b option * 'a list
