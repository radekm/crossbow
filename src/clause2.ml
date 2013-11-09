(* Copyright (c) 2013 Radek Micek *)

type id = int

type t = {
  cl_id : id;
  cl_lits : Clause.t;
}
