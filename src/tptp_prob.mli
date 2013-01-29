(* Copyright (c) 2013 Radek Micek *)

(** Problems in TPTP format. *)

(** Note: Data constructors [Number] and [String] are used only for distinct
   constants so their arity is always zero. Data constructor [Atomic_word]
   is never used for distinct constants (but can be used
   for ordinary constants).
*)
type tptp_symbol =
  | Atomic_word of Tptp_ast.atomic_word * Symb.arity
  | Number of Q.t
  | String of Tptp_ast.tptp_string

type symb_map = {
  of_tptp : (tptp_symbol, Symb.id) Hashtbl.t;
  to_tptp : (Symb.id, tptp_symbol) Hashtbl.t;
}

(** Note: [preds.(s)] tells if the symbol [s] is used as a predicate or
   as a function.
*)
type t = {
  smap : symb_map;
  preds : (tptp_symbol, bool) Hashtbl.t;
  prob : Prob.t;
}

(** Adds TPTP clause to the problem. *)
val add_clause : t -> Tptp_ast.cnf_formula -> unit

(** [of_file base_dir file] reads a problem in the TPTP format from the given
   file [file] and files which are included from [file]. Every file should
   be given by a path relative to [base_dir].
*)
val of_file : string -> string -> t
