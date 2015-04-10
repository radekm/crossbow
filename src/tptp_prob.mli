(* Copyright (c) 2013-15 Radek Micek *)

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

(** [restrict_symb_map symbs smap] creates a new [symb_map] which is
   the restriction of [smap] to [symbs].
*)
val restrict_symb_map : Symb.Set.t -> symb_map -> symb_map

type t = {
  smap : symb_map;
  prob : [`R|`W] Prob.t;
  has_conjecture : bool;
}

(** How to use another program to process clauses:

   1. Use [prob_to_tptp] to give clauses to the program.
   [prob_to_tptp] returns a set of symbols which occur in the clauses
   given to the program, let's call this set [given_symbs].

   2. Use [clauses_of_tptp] to read the clauses returned by the program.

   The program may introduce new symbols whose names will coincide
   with already existing symbols which are not in [given_symbs].
   To ensure that these new symbols get new entries in [symdb]
   call [clauses_of_tptp] with [smap] restricted to [given_symbs]
   (with the restricted [smap] the symbols introduced by the program
   won't be translated to existing symbols).
   The restricted [smap] can be thrown away
   after [clauses_of_tptp] completes. The restricted [smap]
   can be obtained by [restrict_symb_map given_symbs smap].

   If [smap] is not restricted to [given_symbs]
   and the program introduces a new predicate symbol
   with a name of an existing function symbol (or the other way aroud)
   then an exception will be raised when translating
   a clause with that symbol from TPTP.
*)
val clauses_of_tptp :
  [> `W] Symb.db ->
  symb_map ->
  Tptp_ast.tptp_input list ->
  Clause.t list

(** [of_file clausify base_dir file] reads a problem in the TPTP format
   from the given file [file] and the files which are included from [file].
   Relative include paths are resolved against [base_dir].

   Symbols introduced by clausification are auxiliary.

   Note: [base_dir] does not affect the path to the file [file].
*)
val of_file :
  (Tptp_ast.tptp_input list -> Tptp_ast.tptp_input list) ->
  string -> string -> t

type commutativity =
  | Export
  | Export_flat

(** TPTP symbol names will be generated and stored in [smap] if necessary.

   Clauses for commutativity are generated only for the symbols
   which occur in the clauses.

   Returns the set of the symbols which occur in the clauses.
*)
val prob_to_tptp :
  t ->
  commutativity ->
  (Tptp_ast.tptp_input -> unit) ->
  Symb.Set.t

(** TPTP symbol names will be generated and stored in [smap] if necessary. *)
val clause_to_string : [> `R] Symb.db -> symb_map -> Clause.t -> string

(** [model_to_tptp p m interp_name f] converts the given model [m]
   to the TPTP format and calls [f] for each of its formulas.
   [interp_name] is used as a name for all formulas.

   The model [m] must interpret all distinct constants.
   TPTP symbol names will be generated and stored in [smap] if necessary.

   Optional parameters [role_dom], [role_pred] and [role_func]
   affect the roles of formulas (default arguments are
   [Tptp_ast.R_fi_domain], [Tptp_ast.R_fi_predicates] and
   [Tptp_ast.R_fi_functors]).
*)
val model_to_tptp :
  ?prob_name:string ->
  ?role_dom:Tptp_ast.formula_role ->
  ?role_pred:Tptp_ast.formula_role ->
  ?role_func:Tptp_ast.formula_role ->
  t ->
  Model.t ->
  Tptp_ast.formula_name ->
  (Tptp_ast.tptp_input -> unit) ->
  unit
