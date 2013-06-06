(* Copyright (c) 2013 Radek Micek *)

type table = {
  param_sizes : int array;
  values : int array;
}

type 's t = {
  max_size : int;
  symbs : ('s Symb.id, table) Hashtbl.t;
}
