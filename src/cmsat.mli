(* Copyright (c) 2013 Radek Micek *)

(** Binding to CryptoMiniSat. *)

(** Represents an instance of the CryptoMiniSat solver. *)
type t

type var = int

type lit = private int

(** Creates a new solver. *)
external create : unit -> t = "cmsat_create"

(** Creates a new variable. *)
external new_var : t -> var = "cmsat_new_var"

(** [add_clause s lits n] adds the clause containing the first [n] literals
   from [lits].
*)
external add_clause : t -> (lit, [> `R]) Earray.t -> int -> bool =
  "cmsat_add_clause"

(** Starts the solver with the assumptions.
   All variables are assigned if the model is found.
*)
external solve : t -> (lit, [> `R]) Earray.t -> Sh.lbool = "cmsat_solve"

external model_value : t -> var -> Sh.lbool = "cmsat_model_value"

external interrupt : t -> unit = "cmsat_interrupt"

val to_lit : Sh.sign -> var -> lit

val to_var : lit -> var
