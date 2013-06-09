(* Copyright (c) 2013 Radek Micek *)

(** Literals. *)

type 's t =
  | Lit of Sh.sign * 's Symb.id * 's Term.t array

(** Constructs an equality. *)
val mk_eq : 's Term.t -> 's Term.t -> 's t

(** Constructs an inequality. *)
val mk_ineq : 's Term.t -> 's Term.t -> 's t

(** [neg l] negates the literal [l]. *)
val neg : 's t -> 's t

(** Returns [true] iff the literal is an equality of two identical terms. *)
val is_true : 's t -> bool

(** Returns [true] iff the literal is an inequality of two identical terms. *)
val is_false : 's t -> bool

val contains : 's Term.t -> 's t -> bool

val iter : ('s Term.t -> unit) -> 's t -> unit

val vars : 's t -> BatSet.IntSet.t

val lift : ('s Term.t -> 's Term.t) -> 's t -> 's t

val normalize_comm : 's Symb.db -> 's t -> 's t

val replace : 's Term.t -> 's Term.t -> 's t -> 's t

val show : 's t -> string
