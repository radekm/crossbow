(* Copyright (c) 2013 Radek Micek *)

(** Binding to Josat. *)

(** Represents an instance of the Josat solver. *)
type t

type var = int

type lit = private int

(** Creates a new solver. *)
external create : unit -> t = "josat_create"

(** Creates a new variable. *)
external new_var : t -> var = "josat_new_var"

(** [add_clause s lits n] adds the clause containing the first [n] literals
   from [lits].
*)
external add_clause : t -> lit array -> int -> bool = "josat_add_clause"

(** Starts the solver with the assumptions.
   All variables are assigned if the model is found.
*)
external solve : t -> lit array -> Sh.lbool = "josat_solve"

external model_value : t -> var -> Sh.lbool = "josat_model_value"

external interrupt : t -> unit = "josat_interrupt"

external clear_interrupt : t -> unit = "josat_clear_interrupt"

val to_lit : Sh.sign -> var -> lit

val to_var : lit -> var
