(* Copyright (c) 2013 Radek Micek *)

(** Sort inference. *)

type sort_id = int

(** Represents a many-sorted signature with information
   about adequate domain sizes and constants.

   The fields [symb_sorts] and [var_sorts] represent a many-sorted signature.
   [symb_sorts] associates each predicate symbol with the sorts of its
   parameters and each function symbol with the sorts of its parameters
   and the sort of its result. Predicate symbols must be disjoint
   from function symbols.

   The domain size [k] of a sort [A] is said to be adequate iff
   the following conditions hold:

   - Any finite model where the sort [A] has the domain size [n] and [n >= k]
     can be {b extended} in a polynomial time to another finite model where
     the sort [A] has the domain size [n+1] and the other domain sizes
     remain unchanged.
   - Any finite model where the sort [A] has the domain size [n] and [n > k]
     can be {b reduced} in a polynomial time to another finite model where
     the sort [A] has the domain size [n-1] and the other domain sizes
     remain unchanged.

   The array [adeq_sizes] contains an adequate domain size for each sort or
   zero if no adequate domain size was found.
*)
type 's t = {
  symb_sorts : ('s Symb.id, sort_id array) Hashtbl.t;
  (** Sorts of predicate and function symbols. *)

  var_sorts : (Clause2.id * Term.var, sort_id) Hashtbl.t;
  (** Sorts of variables. *)

  adeq_sizes : int array;
  (** [adeq_sizes.(i)] is the adequate domain size of the sort [i]. *)

  consts : 's Symb.id array array;
  (** [consts.(i)] is an array of the constants of the sort [i]. *)

  only_consts : bool ref;
  (** [true] iff [symb_sorts] contain no function symbol with arity > 0. *)
}

(** Infers sorts and finds adequate domain sizes.

   Let [k] be the number of  the constants of a sort [A].
   There are two situations when the adequate domain size
   for the sort [A] is found:

   - If there is no function symbol of arity > 0 having [A] as its result sort
     and no literal [x = y] and no literal [x = f] where [x] has sort [A] then
     [max k 1] is an adequate domain size.
   - If there is no function symbol of arity > 0 having [A] as its result sort
     and no literal [x = y] where [x] has sort [A] then
     [k + 1] is an adequate domain size.
*)
val of_problem : 's Prob.t -> 's t
