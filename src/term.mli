(* Copyright (c) 2013 Radek Micek *)

(** Terms. *)

type var = int

(** Note: terms should not be modified. *)
type t = private
  | Var of var
  | Func of Symb.id * (t, [`R]) Earray.t

val var : var -> t

val func : Symb.id * (t, [> `R]) Earray.t -> t

val is_var : t -> bool

val is_func : t -> bool

val is_const : t -> bool

val is_proper_func : t -> bool

val get_args : t -> (t, [`R]) Earray.t

(** [contains sub t] tests whether the term [t] contains [sub] as a subterm. *)
val contains : t -> t -> bool

val is_ground : t -> bool

(** [iter f t] successively applies [f] to all subterms of [t].
   [f] is always applied to a parent term before it is applied to its child terms.
*)
val iter : (t -> unit) -> t -> unit

(** Counts the number of function symbols in the given term. *)
val count_symbs : t -> int

(** Reorders arguments of commutative symbols in a such way that
    the first one is not smaller than the second one.
*)
val normalize_comm : [> `R] Symb.db -> t -> t

(** [replace a b t] replaces all occurences of the term [a] in [t] by [b]. *)
val replace : t -> t -> t -> t

(** Returns the set of the variables in the given term. *)
val vars : t -> Sh.IntSet.t

(** Converts term to string. *)
val show : t -> string
