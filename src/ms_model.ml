(* Copyright (c) 2013 Radek Micek *)

type table = {
  param_sizes : int array;
  values : int array;
}

type t = {
  max_size : int;
  symbs : (Symb.id, table) Hashtbl.t;
}
