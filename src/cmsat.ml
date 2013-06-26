(* Copyright (c) 2013 Radek Micek *)

type t

type var = int

type lit = int

external create : unit -> t = "cmsat_create"

external new_var : t -> var = "cmsat_new_var"

external add_clause : t -> lit array -> int -> bool = "cmsat_add_clause"

external solve : t -> lit array -> Sh.lbool = "cmsat_solve"

external model_value : t -> var -> Sh.lbool = "cmsat_model_value"

external interrupt : t -> unit = "cmsat_interrupt"

let to_lit sign v = match sign with
  | Sh.Pos -> v + v
  | Sh.Neg -> v + v + 1

let to_var lit = lit / 2
