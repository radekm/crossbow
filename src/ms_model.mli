(* Copyright (c) 2013 Radek Micek *)

(** Multi-sorted models. *)

type table = {
  param_sizes : int array;
  (** Domain sizes of parameters. *)
  values : int array;
}

type t = {
  max_size : int;
  symbs : (Symb.id, table) Hashtbl.t;
}
