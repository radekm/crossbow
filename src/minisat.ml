(* Copyright (c) 2013 Radek Micek *)

type t

type lbool =
  | Ltrue
  | Lfalse
  | Lundef

type var = int

type lit = int

type sign =
  | Pos
  | Neg

external create : unit -> t = "minisat_create"

external new_var : t -> var = "minisat_new_var"

external add_clause : t -> lit array -> int -> bool = "minisat_add_clause"

external solve : t -> lit array -> lbool = "minisat_solve"

external model_value : t -> var -> lbool = "minisat_model_value"

external interrupt : t -> unit = "minisat_interrupt"

external clear_interrupt : t -> unit = "minisat_clear_interrupt"

let to_lit sign v = match sign with
  | Pos -> v + v
  | Neg -> v + v + 1

let to_var lit = lit / 2
