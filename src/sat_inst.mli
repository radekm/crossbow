(* Copyright (c) 2013 Radek Micek *)

(** Instantiation of flat clauses for SAT solvers. *)

(** SAT solver for instantiation. *)
module type Solver = sig
  type t

  type lbool =
    | Ltrue
    | Lfalse
    | Lundef

  type var = int

  type lit = private int

  type sign =
    | Pos
    | Neg

  (** Initializes a solver. *)
  val create : unit -> t

  (** Creates a fresh propositional variable. *)
  val new_var : t -> var

  (** Creates a fresh propositional variable which is always false.

     These variables are used for marking "at least one value"
     clauses which are valid only for one specific domain size.
  *)
  val new_false_var : t -> var

  val add_clause : t -> lit array -> int -> bool

  val add_symmetry_clause : t -> lit array -> int -> bool

  val add_at_least_one_val_clause : t -> lit array -> int -> bool

  val add_at_most_one_val_clause : t -> lit array -> bool

  val remove_clauses_with_lit : t -> lit -> unit

  (** Starts the solver with the given assumptions. *)
  val solve : t -> lit array -> lbool

  val model_value : t -> var -> lbool

  val to_lit : sign -> var -> lit

  val to_var : lit -> var
end

(** Instantiation for SAT solvers. *)
module type Inst_sig = sig
  type lbool
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
  val create : Prob.t -> Sorts.t -> t

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
  val solve : t -> lbool

  (** Returns the solver instance. *)
  val get_solver : t -> solver

  (** Returns the current maximum domain size. *)
  val get_max_size : t -> int
end

module Make (Solv : Solver) :
  Inst_sig with type lbool = Solv.lbool
           and type solver = Solv.t
