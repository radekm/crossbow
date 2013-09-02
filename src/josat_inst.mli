(* Copyright (c) 2013 Radek Micek *)

(** Instantiation for Josat. *)

(** Josat solver. *)
module Josat_ex : Sat_inst.Solver

(** Instantiation for Josat. *)
module Inst :
  Sat_inst.Inst_sig with type solver = Josat_ex.t
