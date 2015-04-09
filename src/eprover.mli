(* Copyright (c) 2014-15 Radek Micek *)

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
