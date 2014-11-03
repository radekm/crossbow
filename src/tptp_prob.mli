(* Copyright (c) 2013-14 Radek Micek *)

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

(** Mapping between symbols from module [Symb] and TPTP symbols. *)
type symb_map = {
  of_tptp : (tptp_symbol, Symb.id) Hashtbl.t;
  to_tptp : (Symb.id, tptp_symbol) Hashtbl.t;
}

type t = {
  smap : symb_map;
  prob : [`R|`W] Prob.t;
}

val clauses_of_tptp :
  [> `W] Symb.db ->
  symb_map ->
  Tptp_ast.tptp_input list ->
  Clause.t list

(** [of_file base_dir file] reads a problem in the TPTP format from the given
   file [file] and the files which are included from [file].
   Relative include paths are resolved against [base_dir].

   Symbols introduced by clausification are auxiliary.

   Note: [base_dir] does not affect the path to the file [file].
*)
val of_file : string -> string -> t

type commutativity =
  | Export
  | Export_flat

(** New TPTP symbols will be generated and stored in [smap] if necessary.

   Clauses for commutativity are generated only for the symbols
   which occur in the clauses.
*)
val prob_to_tptp :
  t ->
  commutativity ->
  (Tptp_ast.tptp_input -> unit) ->
  unit

(** New TPTP symbols will be generated and stored in [smap] if necessary. *)
val clause_to_string : [> `R] Symb.db -> symb_map -> Clause.t -> string

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
  t ->
  Model.t ->
  Tptp_ast.formula_name ->
  (Tptp_ast.tptp_input -> unit) ->
  unit
