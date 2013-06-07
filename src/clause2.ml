(* Copyright (c) 2013 Radek Micek *)

type id = int

type 's t = {
  cl_id : id;
  cl_lits : 's Clause.t;
}
