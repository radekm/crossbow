(* Copyright (c) 2013 Radek Micek *)

(** Literals. *)

type t = private
  | Lit of Sh.sign * Symb.id * Term.t array

val lit : Sh.sign * Symb.id * Term.t array -> t

(** Constructs an equality. *)
val mk_eq : Term.t -> Term.t -> t

(** Constructs an inequality. *)
val mk_ineq : Term.t -> Term.t -> t

(** [neg l] negates the literal [l]. *)
val neg : t -> t

(** Returns [true] iff the literal is an equality of two identical terms. *)
val is_true : t -> bool

(** Returns [true] iff the literal is an inequality of two identical terms. *)
val is_false : t -> bool

val contains : Term.t -> t -> bool

val iter : (Term.t -> unit) -> t -> unit

val vars : t -> Sh.IntSet.t

val lift : (Term.t -> Term.t) -> t -> t

val normalize_comm : [> `R] Symb.db -> t -> t

val replace : Term.t -> Term.t -> t -> t

val show : t -> string
