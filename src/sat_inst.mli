(* Copyright (c) 2013 Radek Micek *)

(** Instantiation of flat clauses for SAT solvers. *)

(** SAT solver for instantiation. *)
module type Solver = sig
  include Sat_solver.S

  (** Creates a fresh propositional variable which is always false.

     These variables are used for marking "at least one value"
     clauses which are valid only for one specific domain size.
  *)
  val new_false_var : t -> var

  val add_symmetry_clause : t -> lit array -> int -> bool

  val add_at_least_one_val_clause : t -> lit array -> int -> bool

  val add_at_most_one_val_clause : t -> lit array -> bool

  val remove_clauses_with_lit : t -> lit -> unit
end

(** Instantiation for SAT solvers. *)
module type Inst_sig = sig
  type solver

  type t

  (** Initialization:

     - Creates a solver.
     - Creates propositional variables for the nullary predicates.
     - Preprocesses the clauses.
     - Instantiates the clauses without variables.

     Important: input must not be changed after this call!

     Note: the maximum domain size is 0.
  *)
  val create : [> `R] Prob.t -> Sorts.t -> t

  (** Increases the maximum domain size:

     - Creates propositional variables.
     - Removes old "at least one value" clauses.
     - Adds symmetry reduction clauses.
     - Adds "at most one value" clauses.
     - Instantiates the clauses with variables.

    Note: "at least one value" clauses are not added.
  *)
  val incr_max_size : t -> unit

  (** Adds "at least one value" clauses and starts the solver.

     Raises [Failure] when the maximum domain size is 0 or when it is
     lower than the number of the distinct constants.
  *)
  val solve : t -> Sh.lbool

  (** Adds "at least one value" clauses and starts the solver.
     The solver is interrupted if it doesn't finish within
     the given number of miliseconds. Returns the solver result
     and the indicator whether the solver was interrupted.

     Raises [Failure] when the maximum domain size is 0 or when it is
     lower than the number of the distinct constants.
  *)
  val solve_timed : t -> int -> Sh.lbool * bool

  (** Constructs a multi-sorted model for all constants, non-auxiliary
     functions and non-auxiliary predicates.

     Can be used immediately after the successful call to {!solve}.
     Raises [Failure] when used after the call to {!incr_max_size}
     or after the unsuccessful call to {!solve}.
  *)
  val construct_model : t -> Ms_model.t

  (** Blocks every model which is an extension of the given model. *)
  val block_model : t -> Ms_model.t -> unit

  (** Returns the solver instance. *)
  val get_solver : t -> solver

  (** Returns the current maximum domain size. *)
  val get_max_size : t -> int
end

module Make (Solv : Solver) :
  Inst_sig with type solver = Solv.t
