(* Copyright (c) 2013 Radek Micek *)

(** Assignment generation and ranking. *)

(** Holds an assignment.

   {b Definition:} Let [max_size > 0] be a domain size, [adeq_sizes]
   be an array of adequate domain sizes (i.e. elements are nonnegative)
   and [a] be an array of integers.

   A subarray [[|a.(i0); ..; a.(n)|]] of [a] is
   an {e assignment wrt [adeq_sizes]} if
   [0 <= a.(j) && (adeq_sizes.(j) = 0 || a.(j) < adeq_sizes.(j))]
   holds for each [j = i0,..,n].

   A subarray [[|a.(i0); ..; a.(n)|]] of [a] is
   an {e assignment wrt [max_size] and [adeq_sizes]} if [0 <= a.(j) < dsize j]
   holds for each [j = i0,..,n] where
{[
let dsize j =
  if adeq_sizes.(j) = 0 || adeq_sizes.(j) >= max_size
  then max_size
  else adeq_size.(j)
]}

   {b Definition:} Assignments [[|a.(i0); ..; a.(n)|]] and
   [[|b.(i0); ..; b.(n)|]] are {e equivalent up to commutativity} if
   they are same or if they are same after swapping [a.(i0)] with [a.(i0+1)].
*)
type t = int array

(** {6 Generation} *)

(** [each a start len adeq_sizes max_size f] consecutively generates all
   assignments [[|a.(start); ..; a.(start+len-1)|]] wrt [max_size] and [adeq_sizes]
   and calls [f a] for each generated assignment.
*)
val each : t -> int -> int -> int array -> int -> (t -> unit) -> unit

(** [each_me a start len adeq_sizes max_size f] consecutively generates all
   assignments [[|a.(start); ..; a.(start+len-1)|]] wrt [max_size] and
   [adeq_sizes] which have at least one occurence of [max_el = max_size-1]
   and calls [f a] for each generated assignment.
*)
val each_me : t -> int -> int -> int array -> int -> (t -> unit) -> unit

(** [each_comm_me a start len adeq_sizes max_size f] consecutively generates
   all assignments [[|a.(start); ..; a.(start+len-1)|]] wrt [max_size] and
   [adeq_sizes] which are not equivalent up to commutativity
   and which have at least one occurence of [max_el = max_size-1].
   [f a] is called for each generated assignment.

   Note: [len] must be at least 2.
*)
val each_comm_me : t -> int -> int -> int array -> int -> (t -> unit) -> unit

(** {6 Counting} *)

(** [count start len adeq_size max_size] counts the number of the assignments
   [[|a.(start); ..; a.(start+len-1)|]] wrt [max_size] and [adeq_sizes].
*)
val count : int -> int -> int array -> int -> int

(** [count_me start len adeq_size max_size] counts the number
   of the assignments [[|a.(start); ..; a.(start+len-1)|]] wrt [max_size]
   and [adeq_sizes] which have at least one occurence of [max_el = max_size-1].
*)
val count_me : int -> int -> int array -> int -> int

(** [count_comm_me start len adeq_size max_size] counts the number
   of the assignments [[|a.(start); ..; a.(start+len-1)|]] wrt [max_size]
   and [adeq_sizes] which are not equivalent up to commutativity and
   which have at least one occurence of [max_el = max_size-1].

   Note: [len] must be at least 2.
*)
val count_comm_me : int -> int -> int array -> int -> int

(** {6 Ranking} *)

type rank = int

(** [rank_me a start len adeq_sizes] returns [(r, max_el_idx)].
   [max_el_idx] is the index of the leftmost occurence of the
   maximal element [max_el] in the assignment. [r] is the ordinal number
   of the assignment in the sequence of the assignments generated by
   [each_me a start len adeq_sizes (max_el+1) f].
*)
val rank_me : t -> int -> int -> int array -> rank * int

(** [rank_comm_me a start len adeq_sizes] returns [(r, max_el_idx)].
   [max_el_idx] is the index of the leftmost occurence of the
   maximal element [max_el] in the assignment. [r] is the ordinal number
   of the assignment in the sequence of the assignments generated by
   [each_comm_me a start len adeq_sizes (max_el+1) f].

   Note: [len] must be at least 2.
*)
val rank_comm_me : t -> int -> int -> int array -> rank * int
