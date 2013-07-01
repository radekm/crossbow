(* Copyright (c) 2013 Radek Micek *)

(** Instantiation of clauses for CSP solvers. *)

(** Instantiation for CSP solvers. *)
module type Inst_sig = sig
  type solver
  type 's t

  (** [create ~symmetric_vals ~nthreads prob n] instantiates
     the problem [prob] for the domain size [n].
     [symmetric_vals] bigger than zero means that values
     [0..symmetric_vals-1] are symmetrical. [nthreads] is the number
     of threads to use when solving.
  *)
  val create :
    ?symmetric_vals:int -> ?nthreads:int -> 's Prob.t -> int -> 's t

  val solve : 's t -> Sh.lbool

  val construct_model : 's t -> 's Model.t

  val get_solver : 's t -> solver
end

module Make (Solv : Csp_solver.S) :
  Inst_sig with type solver = Solv.t
