(* Copyright (c) 2013 Radek Micek *)

type t

type var = int

type lit = int

external create : unit -> t = "cmsat_create"

external new_var : t -> var = "cmsat_new_var"

external add_clause : t -> lit array -> int -> bool = "cmsat_add_clause"

external solve : t -> lit array -> Sat_solver.lbool = "cmsat_solve"

external model_value : t -> var -> Sat_solver.lbool = "cmsat_model_value"

let to_lit sign v = match sign with
  | Sat_solver.Pos -> v + v
  | Sat_solver.Neg -> v + v + 1

let to_var lit = lit / 2
