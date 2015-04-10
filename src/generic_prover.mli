(* Copyright (c) 2015 Radek Micek *)

(** Support for external provers. *)

(** [run exe opts inputs] executes an external prover. The prover
   is executed by the command [exe opts input] where [input] is a TPTP file
   with [inputs] and prints TPTP output to its standard output.

   Note: [run] doesn't support E since its output isn't valid TPTP.
*)
val run :
  string ->
  string list ->
  Tptp_ast.tptp_input list ->
  Tptp_ast.tptp_input list
