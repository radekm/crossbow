(* Copyright (c) 2013 Radek Micek *)

(** Instantiation for MiniSat. *)

(** MiniSat solver. *)
module Minisat_ex : Sat_inst.Solver

(** Instantiation for MiniSat. *)
module Inst :
  Sat_inst.Inst_sig with type lbool = Minisat_ex.lbool
                    and type solver = Minisat_ex.t
