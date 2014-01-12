(* Copyright (c) 2013 Radek Micek *)

(** Array with capabilities. *)

(** Represents array with read and write capabilities. *)
type ('a,'c) t constraint 'c = [< `R|`W]

(** {4 BatArray functions} *)

(** {6 Array operations} *)

external length : ('a, [> ]) t -> int = "%array_length"

external get : ('a, [> `R]) t -> int -> 'a = "%array_safe_get"
external set : ('a, [> `W]) t -> int -> 'a -> unit = "%array_safe_set"

external make : int -> 'a -> ('a, _) t = "caml_make_vect"

val init : int -> (int -> 'a) -> ('a, _) t

val append : ('a, [> `R]) t -> ('a, [> `R]) t -> ('a, _) t

val concat : ('a, [> `R]) t list -> ('a, _) t

val sub : ('a, [> `R]) t -> int -> int -> ('a, _) t

val copy : ('a, [> `R]) t -> ('a, _) t

val fill : ('a, [> `W]) t -> int -> int -> 'a -> unit

val blit : ('a, [> `R]) t -> int -> ('a, [> `W]) t -> int -> int -> unit

val to_list : ('a, [> `R]) t -> 'a list

val of_list : 'a list -> ('a, _) t

val max : ('a, [> `R]) t -> 'a

val min : ('a, [> `R]) t -> 'a

val sum : (int, [> `R]) t -> int

val fsum : (float, [> `R]) t -> float

val avg : (int, [> `R]) t -> float

val favg : (float, [> `R]) t -> float

val left : ('a, [> `R]) t -> int -> ('a, _) t

val right : ('a, [> `R]) t -> int -> ('a, _) t

val head : ('a, [> `R]) t -> int -> ('a, _) t

val tail : ('a, [> `R]) t -> int -> ('a, _) t

val iter : ('a -> unit) -> ('a, [> `R]) t -> unit

val map : ('a -> 'b) -> ('a, [> `R]) t -> ('b, _) t

val iteri : (int -> 'a -> unit) -> ('a, [> `R]) t -> unit

val mapi : (int -> 'a -> 'b) -> ('a, [> `R]) t -> ('b, _) t

val fold_left : ('a -> 'b -> 'a) -> 'a -> ('b, [> `R]) t -> 'a

val fold_right : ('b -> 'a -> 'a) -> ('b, [> `R]) t -> 'a -> 'a

val modify : ('a -> 'a) -> ('a, [`R|`W]) t -> unit

val modifyi : (int -> 'a -> 'a) -> ('a, [`R|`W]) t -> unit

val fold_lefti : ('a -> int -> 'b -> 'a) -> 'a -> ('b, [> `R]) t -> 'a

val fold_righti : (int -> 'b -> 'a -> 'a) -> ('b, [> `R]) t -> 'a -> 'a

val reduce : ('a -> 'a -> 'a) -> ('a, [> `R]) t -> 'a

val singleton : 'a -> ('a, _) t

(** {6 Sorting} *)

val sort : ('a -> 'a -> int) -> ('a, [`R|`W]) t -> unit

val stable_sort : ('a -> 'a -> int) -> ('a, [`R|`W]) t -> unit

val fast_sort : ('a -> 'a -> int) -> ('a, [`R|`W]) t -> unit

val decorate_stable_sort : ('a -> 'b) -> ('a, [> `R]) t -> ('a, _) t

val decorate_fast_sort : ('a -> 'b) -> ('a, [> `R]) t -> ('a, _) t


(** {6 Operations on two arrays} *)

val iter2 : ('a -> 'b -> unit) -> ('a, [> `R]) t -> ('b, [> `R]) t -> unit

val iter2i :
  (int -> 'a -> 'b -> unit) -> ('a, [> `R]) t -> ('b, [> `R]) t -> unit

val for_all2 : ('a -> 'b -> bool) -> ('a, [> `R]) t -> ('b, [> `R]) t -> bool

val exists2 : ('a -> 'b -> bool) -> ('a, [> `R]) t -> ('b, [> `R]) t -> bool

val map2 : ('a -> 'b -> 'c) -> ('a, [> `R]) t -> ('b, [> `R]) t -> ('c, _) t

(** {6 Predicates} *)

val for_all : ('a -> bool) -> ('a, [> `R]) t -> bool

val exists : ('a -> bool) -> ('a, [> `R]) t -> bool

val find : ('a -> bool) -> ('a, [> `R]) t -> 'a

val mem : 'a -> ('a, [> `R]) t -> bool

val memq : 'a -> ('a, [> `R]) t -> bool

val findi : ('a -> bool) -> ('a, [> `R]) t -> int

val filter : ('a -> bool) -> ('a, [> `R]) t -> ('a, _) t

val filteri : (int -> 'a -> bool) -> ('a, [> `R]) t -> ('a, _) t

val filter_map : ('a -> 'b option) -> ('a, [> `R]) t -> ('b, _) t

val find_all : ('a -> bool) -> ('a, [> `R]) t -> ('a, _) t

val partition : ('a -> bool) -> ('a, [> `R]) t -> ('a, _) t * ('a, _) t

(** {6 Array transformations} *)

val rev : ('a, [> `R]) t -> ('a, _) t

val rev_in_place : ('a, [`R|`W]) t -> unit

(** {6 Conversions} *)

val enum : ('a, [> `R]) t -> 'a BatEnum.t

val of_enum : 'a BatEnum.t -> ('a, _) t

val backwards : ('a, [> `R]) t -> 'a BatEnum.t

val of_backwards : 'a BatEnum.t -> ('a, _) t


(** {6 Utilities} *)

val range : ('a, [> `R]) t -> int BatEnum.t

val insert : ('a, [> `R]) t -> 'a -> int -> ('a, _) t


(** {6 Boilerplate code} *)

val print :
  ?first:string -> ?last:string -> ?sep:string ->
  ('a, 'b) BatIO.printer -> (('a, [> `R]) t, 'b) BatIO.printer

val compare : 'a BatOrd.comp -> ('a, [> `R]) t BatOrd.comp

val ord : 'a BatOrd.ord -> ('a, [> `R]) t BatOrd.ord

val equal : 'a BatOrd.eq -> ('a, [> `R]) t BatOrd.eq

module Exceptionless : sig
  val find : ('a -> bool) -> ('a, [> `R]) t -> 'a option

  val findi : ('a -> bool) -> ('a, [> `R]) t -> int option
end

(** {4 Extra functions} *)

val empty : ('a, _) t

val is_empty : ('a, [> `R]) t -> bool

external of_array : 'a array -> ('a, _) t = "%identity"

external to_array : ('a, [`R|`W]) t -> 'a array = "%identity"

val of_dyn_array : 'a BatDynArray.t -> ('a, _) t

external read_only : ('a, [> `R]) t -> ('a, [`R]) t = "%identity"
external write_only : ('a, [> `W]) t -> ('a, [`W]) t = "%identity"

(** [pick f a] successively applies [f] to the elements of [a] until [f]
   succeeds. The result of [f] is returned if [f] succeeded, [None] otherwise.
*)
val pick : ('a -> 'b option) -> ('a, [> `R]) t -> 'b option

(** [existsi p arr] checks if at least one element of
   the array satisfies the predicate [p]. [p] takes an element and its index.
*)
val existsi : (int -> 'a -> bool) -> ('a, [> `R]) t -> bool

(** [rindex_of p a start len] returns the index of the rightmost element
   among [a.(start), .., a.(start+len-1)] which satisfies [p].
   [p] takes an element and its index.
*)
val rindex_of :
  (int -> 'a -> bool) -> ('a, [> `R]) t -> int -> int -> int option

(** [iter_combinations f k arr] applies [f] to each [k]-combination
   of the elements of [arr]. The array in which the [k]-combinations
   are passed to [f] is the same for all calls to [f].
*)
val iter_combinations : (('a, [`R]) t -> unit) -> int -> ('a, [`R]) t -> unit

(** [iter_permutations f arr] applies [f] to each permutation of [arr].
   The permutations are generated in the lexicographic order. The array
   in which the permutations are passed to [f] is the same
   for all calls to [f]. The elements of [arr] must be distinct.
*)
val iter_permutations : (('a, [`R]) t -> unit) -> ('a, [`R]) t -> unit

(** {4 Syntax} *)

module Array : sig
  external get : ('a, [> `R]) t -> int -> 'a = "%array_safe_get"
  external set : ('a, [> `W]) t -> int -> 'a -> unit = "%array_safe_set"
end
