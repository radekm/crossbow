(* Copyright (c) 2013 Radek Micek *)

(** Binding to MiniSat. *)

(** Represents an instance of the MiniSat solver. *)
type t

type var = int

type lit = private int

(** Creates a new solver. *)
external create : unit -> t = "minisat_create"

(** Creates a new variable. *)
external new_var : t -> var = "minisat_new_var"

(** [add_clause s lits n] adds the clause containing the first [n] literals
   from [lits].
*)
external add_clause : t -> lit array -> int -> bool = "minisat_add_clause"

(** Starts the solver with the assumptions.
   All variables are assigned if the model is found.
*)
external solve : t -> lit array -> Sat_solver.lbool = "minisat_solve"

external model_value : t -> var -> Sat_solver.lbool = "minisat_model_value"

external interrupt : t -> unit = "minisat_interrupt"

external clear_interrupt : t -> unit = "minisat_clear_interrupt"

val to_lit : Sh.sign -> var -> lit

val to_var : lit -> var
