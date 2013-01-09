(* Copyright (c) 2013 Radek Micek *)

(** Clauses. *)

(** A literal is an atomic formula or its negation. *)
type lit = Term.t

type t = {
  cl_id : int;
  cl_lits : lit list;
}

(** [neg_lit l] negates the literal [l]. *)
val neg_lit : lit -> lit

(** Returns [true] iff the literal is an equality of two identical terms. *)
val true_lit : lit -> bool

(** Returns [true] iff the literal is an inequality of two identical terms. *)
val false_lit : lit -> bool

(** Simplifies the clause. Here is a list of performed simplifications:

   - Every inequality of variables [x != y] is removed
     and [x] is replaced by [y].
   - The order of the arguments of the commutative symbols is normalized.
   - Duplicate literals are removed.
   - Literals which are never true are removed.

   Returns [None] if the clause is tautology.
*)
val simplify : Symb.db -> t -> t option

(** Counts and renumbers the variables in the given clause.
   The variables are assigned numbers [0,..,n-1] where [n]
   is the count of the distinct variables in the clause.
*)
val normalize_vars : t -> t * int