(* Copyright (c) 2013 Radek Micek *)

module type S = sig
  type t
  type 'a var = private int
  type 'a var_array = private int

  val create : int -> t
  val destroy : t -> unit

  val new_bool_var : t -> bool var
  val new_int_var : t -> int -> int var (* domain size *)

  val new_tmp_bool_var : t -> bool var
  val new_tmp_int_var : t -> int -> int var

  val new_bool_var_array : t -> bool var array -> bool var_array
  val new_int_var_array : t -> int var array -> int var_array

  val linear : t -> int var array -> int array -> int -> unit

  val bool_element : t -> bool var_array -> int var -> bool var -> unit
  val int_element : t -> int var_array -> int var -> int var -> unit

  val eq_var_var : t -> int var -> int var -> bool var -> unit
  val eq_var_const : t -> int var -> int -> bool var -> unit

  val lower_eq : t -> int var -> int -> unit

  val precede : t -> int var array -> int array -> unit

  val clause : t-> bool var array -> bool var array -> unit

  val all_different : t -> int var array -> unit

  val solve : t -> Sh.lbool

  val interrupt : t -> unit

  val bool_value : t -> bool var -> int
  val int_value : t -> int var -> int
end
