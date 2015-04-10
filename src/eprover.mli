(* Copyright (c) 2014-15 Radek Micek *)

(** E prover support.

   E isn't covered by module [Generic_prover] since output from E
   isn't valid TPTP - it contains comments which start with [#].
*)

val clausify :
  string ->
  string list ->
  Tptp_ast.tptp_input list ->
  Tptp_ast.tptp_input list

val generate_lemmas :
  string ->
  string list ->
  int ->
  Tptp_ast.tptp_input list ->
  Tptp_ast.tptp_input list
