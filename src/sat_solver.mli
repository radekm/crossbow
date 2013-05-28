(* Copyright (c) 2013 Radek Micek *)

(** Shared by all SAT solvers. *)

type lbool =
  | Ltrue
  | Lfalse
  | Lundef

type sign =
  | Pos
  | Neg

module type S = sig
  type t

  type var = int

  type lit = private int

  (** Initializes a solver. *)
  val create : unit -> t

  (** Creates a fresh propositional variable. *)
  val new_var : t -> var

  val add_clause : t -> lit array -> int -> bool

  (** Starts the solver with the given assumptions. *)
  val solve : t -> lit array -> lbool

  val model_value : t -> var -> lbool

  val to_lit : sign -> var -> lit

  val to_var : lit -> var
end
