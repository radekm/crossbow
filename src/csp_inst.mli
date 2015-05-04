(* Copyright (c) 2013, 2015 Radek Micek *)

(** Instantiation of clauses for CSP solvers. *)

(** Instantiation for CSP solvers. *)
module type Inst_sig = sig
  type solver
  type t

  (** [create ~nthreads prob n] instantiates
     the problem [prob] for the domain size [n].
     [nthreads] is the number of threads to use when solving.
  *)
  val create : ?nthreads:int -> [> `R] Prob.t -> Sorts.t -> int -> t

  val destroy : t -> unit

  val solve : t -> Sh.lbool

  val solve_timed : t -> int -> Sh.lbool * bool

  val construct_model : t -> Ms_model.t

  val get_solver : t -> solver
end

module Make (Solv : Csp_solver.S) :
  Inst_sig with type solver = Solv.t
