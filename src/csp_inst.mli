(* Copyright (c) 2013 Radek Micek *)

(** Instantiation of clauses for CSP solvers. *)

(** Instantiation for CSP solvers. *)
module type Inst_sig = sig
  type solver
  type 's t

  (** [create ~nthreads prob n] instantiates
     the problem [prob] for the domain size [n].
     [nthreads] is the number of threads to use when solving.
  *)
  val create : ?nthreads:int -> 's Prob.t -> int -> 's t

  val destroy : 's t -> unit

  val solve : 's t -> Sh.lbool

  val solve_timed : 's t -> int -> Sh.lbool * bool

  val construct_model : 's t -> 's Model.t

  val get_solver : 's t -> solver
end

module Make (Solv : Csp_solver.S) :
  Inst_sig with type solver = Solv.t
