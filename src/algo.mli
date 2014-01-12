(* Copyright (c) 2013 Radek Micek *)

(** Various algorithms. *)

(** [vertex_cover edges k] returns a vertex cover of [edges]
   of the size at most [k]. [None] is returned if no such
   cover exists. The returned cover may not be a minimum cover.

   Raises [Invalid_argument] if [k] is negative.
*)
val vertex_cover : ('a * 'a, [> `R]) Earray.t -> int -> 'a list option

(** Computes a minimum vertex cover by repeatedly calling
   {!vertex_cover} with [k = 0,1,2,..].
*)
val min_vertex_cover : ('a * 'a, [> `R]) Earray.t -> 'a list
