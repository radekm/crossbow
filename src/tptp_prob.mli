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

type 's symb_map = {
  of_tptp : (tptp_symbol, 's Symb.id) Hashtbl.t;
  to_tptp : ('s Symb.id, tptp_symbol) Hashtbl.t;
}

(** Note: [preds.(s)] tells if the symbol [s] is used as a predicate or
   as a function.
*)
type 's t = {
  smap : 's symb_map;
  preds : (tptp_symbol, bool) Hashtbl.t;
  prob : 's Prob.t;
}

type wt =
  | Wr : 's t -> wt

(** Adds TPTP clause to the problem. *)
val add_clause : 's t -> Tptp_ast.cnf_formula -> unit

(** [of_file base_dir file] reads a problem in the TPTP format from the given
   file [file] and the files which are included from [file].
   Relative include paths are resolved against [base_dir].

   Note: [base_dir] does not affect the path to the file [file].
*)
val of_file : string -> string -> wt

(** [model_to_tptp p m interp_name f] converts the given model [m]
   to the TPTP format and calls [f] for each of its formulas.
   [interp_name] is used as a name for all formulas.

   Optional parameters [role_dom], [role_pred] and [role_func]
   affect the roles of formulas (default arguments are
   [Tptp_ast.R_fi_domain], [Tptp_ast.R_fi_predicates] and
   [Tptp_ast.R_fi_functors]).
*)
val model_to_tptp :
  ?role_dom:Tptp_ast.formula_role ->
  ?role_pred:Tptp_ast.formula_role ->
  ?role_func:Tptp_ast.formula_role ->
  's t ->
  's Model.t ->
  Tptp_ast.formula_name ->
  (Tptp_ast.tptp_input -> unit) ->
  unit
