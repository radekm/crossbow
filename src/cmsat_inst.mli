(* Copyright (c) 2013 Radek Micek *)

(** Instantiation for CryptoMiniSat. *)

(** CryptoMiniSat solver. *)
module Cmsat_ex : Sat_inst.Solver

(** Instantiation for CryptoMiniSat. *)
module Inst :
  Sat_inst.Inst_sig with type solver = Cmsat_ex.t
