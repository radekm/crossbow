(* Copyright (c) 2013 Radek Micek *)

(** CSP solver. *)

module type S = sig
  (** CSP solver. *)
  type t

  (** CSP variable. *)
  type 'a var = private int

  (** Array of CSP variables for [bool_element] and [int_element]
     constraints.
  *)
  type 'a var_array = private int

  (** [create nthreads] initializes a solver.
     [nthreads] is the number of threads to use when solving.
  *)
  val create : int -> t

  (** Creates a new boolean CSP variable. *)
  val new_bool_var : t -> bool var

  (** [new_int_var s dom_size] creates a new integral CSP variable with
     a domain [0..dom_size-1].
  *)
  val new_int_var : t -> int -> int var

  (** Creates a new temporary boolean CSP variable.
     Temporary variables don't belong to the solution.
  *)
  val new_tmp_bool_var : t -> bool var

  (** Creates a new temporary integral CSP variable.
     Temporary variables don't belong to the solution.
  *)
  val new_tmp_int_var : t -> int -> int var

  (** Creates an array of boolean CSP variables. *)
  val new_bool_var_array : t -> bool var array -> bool var_array

  (** Creates an array of integral CSP variables. *)
  val new_int_var_array : t -> int var array -> int var_array

  (** [linear s vars coefs c] posts constraint
     [vars.(0) * coefs.(0) + vars.(1) * coefs.(1) + ... = c].
     Arrays [vars] and [coefs] must have same length.
  *)
  val linear : t -> int var array -> int array -> int -> unit

  (** [bool_element s vars idx x] posts constraint [vars.(idx) = x]. *)
  val bool_element : t -> bool var_array -> int var -> bool var -> unit

  (** [int_element s vars idx x] posts constraint [vars.(idx) = x]. *)
  val int_element : t -> int var_array -> int var -> int var -> unit

  (** [eq_var_var s x x' b] posts constraint [(x = x') <=> b]. *)
  val eq_var_var : t -> int var -> int var -> bool var -> unit

  (** [eq_var_const s x c b] posts constraint [(x = c) <=> b]. *)
  val eq_var_const : t -> int var -> int -> bool var -> unit

  (** [clause s pos neg] posts constraint
     [pos.(0) || pos.(1) || ... || ~neg.(0) || ~neg.(1) || ...].
  *)
  val clause : t-> bool var array -> bool var array -> unit

  (** [all_different s vars] posts constraints [vars.(i) <> vars.(j)]
     for all [i <> j].
  *)
  val all_different : t -> int var array -> unit

  (** {b Important:} After calling [solve] you must not create CSP variables,
     create arrays of CSP variables, post constraints.
  *)
  val solve : t -> Sh.lbool

  val interrupt : t -> unit

  (** Returns the value of the given non-temporary boolean CSP variable.

     Can be used only when the last call to [solve] returned [Sh.Ltrue].
  *)
  val bool_value : t -> bool var -> int

  (** Returns the value of the given non-temporary integral CSP variable.

     Can be used only when the last call to [solve] returned [Sh.Ltrue].
  *)
  val int_value : t -> int var -> int
end
