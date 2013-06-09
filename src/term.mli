(* Copyright (c) 2013 Radek Micek *)

(** Terms. *)

type var = int

(** Note: terms should not be modified. *)
type 's t =
  | Var of var
  | Func of 's Symb.id * 's t array

val get_args : 's t -> 's t array

(** [contains sub t] tests whether the term [t] contains [sub] as a subterm. *)
val contains : 's t -> 's t -> bool

(** [iter f t] successively applies [f] to all subterms of [t].
   [f] is always applied to a parent term before it is applied to its child terms.
*)
val iter : ('s t -> unit) -> 's t -> unit

(** Reorders arguments of commutative symbols in a such way that
    the first one is not smaller than the second one.
*)
val normalize_comm : 's Symb.db -> 's t -> 's t

(** [replace a b t] replaces all occurences of the term [a] in [t] by [b]. *)
val replace : 's t -> 's t -> 's t -> 's t

(** Returns the set of the variables in the given term. *)
val vars : 's t -> BatSet.IntSet.t

(** Converts term to string. *)
val show : 's t -> string
