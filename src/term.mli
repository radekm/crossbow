(* Copyright (c) 2013 Radek Micek *)

(** Terms. *)

type var = int

(** Note: terms should not be modified. *)
type 's t =
  | Var of var
  | Func of 's Symb.id * 's t array

(** Constructs an equality. *)
val mk_eq : 's t -> 's t -> 's t

(** Constructs an inequality. *)
val mk_ineq : 's t -> 's t -> 's t

(** [contains sub t] tests whether the term [t] contains [sub] as a subterm. *)
val contains : 's t -> 's t -> bool

(** [iter f t] successively applies [f] to all subterms of [t].
   [f] is always applied to a parent term before it is applied to its child terms.
*)
val iter : ('s t -> unit) -> 's t -> unit

(** [pickp f t] successively applies [f] to all subterms of [t] until [f]
   succeeds. The result of [f] is returned if [f] succeeded, [None] otherwise.

   [pickp] traverses [t] in the preorder. [f] takes two arguments [p] and [sub]
   where [sub] is a subterm of [t] and [p] is the parent of [sub].
 *)
val pickp : ('s t option -> 's t -> 'a option) -> 's t -> 'a option

(** Reorders arguments of commutative symbols in a such way that
    the first one is not smaller than the second one.
*)
val normalize_comm : 's Symb.db -> 's t -> 's t

(** [replace a b t] replaces all occurences of the term [a] in [t] by [b]. *)
val replace : 's t -> 's t -> 's t -> 's t

(** Returns the set of the variables in the given term. *)
val vars : 's t -> BatSet.IntSet.t

(** Returns the set of the variables in the given terms. *)
val vars_of_many : 's t list -> BatSet.IntSet.t

(** Converts term to string. *)
val show : 's t -> string
