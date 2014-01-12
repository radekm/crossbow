(* Copyright (c) 2013 Radek Micek *)

module type S = sig
  type t

  type var = int

  type lit = private int

  (** Initializes a solver. *)
  val create : unit -> t

  (** Creates a fresh propositional variable. *)
  val new_var : t -> var

  val add_clause : t -> (lit, [> `R]) Earray.t -> int -> bool

  (** Starts the solver with the given assumptions. *)
  val solve : t -> (lit, [> `R]) Earray.t -> Sh.lbool

  val model_value : t -> var -> Sh.lbool

  val interrupt : t -> unit

  val to_lit : Sh.sign -> var -> lit

  val to_var : lit -> var
end
