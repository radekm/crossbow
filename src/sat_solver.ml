(* Copyright (c) 2013 Radek Micek *)

module type S = sig
  type t

  type var = int

  type lit = private int

  val create : unit -> t

  val new_var : t -> var

  val add_clause : t -> (lit, [> `R]) Earray.t -> int -> bool

  val solve : t -> (lit, [> `R]) Earray.t -> Sh.lbool

  val model_value : t -> var -> Sh.lbool

  val interrupt : t -> unit

  val to_lit : Sh.sign -> var -> lit

  val to_var : lit -> var
end
