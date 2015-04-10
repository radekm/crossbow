(* Copyright (c) 2013-15 Radek Micek *)

open BatPervasives

module Array = Earray.Array
module S = Symb
module T = Term
module L = Lit
module Ast = Tptp_ast

type tptp_symbol =
  | Atomic_word of Ast.atomic_word * Symb.arity
  | Number of Q.t
  | String of Ast.tptp_string

let is_defined_or_system_symb = function
  | Atomic_word (Ast.Plain_word _, _)
  | Number _
  | String _ -> false
  | Atomic_word (Ast.Defined_word _, _)
  | Atomic_word (Ast.System_word _, _) -> true

type symb_map = {
  of_tptp : (tptp_symbol, S.id) Hashtbl.t;
  to_tptp : (S.id, tptp_symbol) Hashtbl.t;
}

let restrict_symb_map symbs smap = {
  of_tptp =
    BatHashtbl.filter
      (fun id -> Symb.Set.mem id symbs)
      smap.of_tptp;
  to_tptp =
    BatHashtbl.filteri
      (fun id _ -> Symb.Set.mem id symbs)
      smap.to_tptp;
}

(* Adds mapping between [symb] and [id] to [smap].

   Defined and system symbols are not allowed.
*)
let add_to_smap smap symb id =
  if is_defined_or_system_symb symb then
    failwith "add_to_smap: Defined or system symbol";
  Hashtbl.add smap.of_tptp symb id;
  Hashtbl.add smap.to_tptp id symb

type t = {
  smap : symb_map;
  prob : [`R|`W] Prob.t;
  has_conjecture : bool;
}

(* Calls [pred] resp. [func] for each occurence of a predicate
   resp. a function symbol in the input formula.

   Predicates [$false/0], [$true/0] and [=/2] are ignored.
   Occurences of symbols in annotations are ignored.
*)
let iter_symbols pred func input =
  let rec proc_term = function
    | Ast.Var _ -> ()
    | Ast.Func (symb, args) ->
        func (Atomic_word (symb, List.length args));
        List.iter proc_term args
    | Ast.Number n -> func (Number n)
    | Ast.String s -> func (String s) in
  let proc_atom = function
    | Ast.Equals (a, b) ->
        proc_term a;
        proc_term b
    | Ast.Pred (Ast.Defined_word s, [])
      when List.mem (s :> string) ["$false"; "$true"] -> ()
    | Ast.Pred (symb, args) ->
        pred (Atomic_word (symb, List.length args));
        List.iter proc_term args in
  let rec proc_formula = function
    | Ast.Binop (_, a, b) ->
        proc_formula a;
        proc_formula b
    | Ast.Not a | Ast.Quant (_, _, a) -> proc_formula a
    | Ast.Atom a -> proc_atom a in
  let proc_lit (Ast.Lit (_, a)) = proc_atom a in

  match input with
    | Ast.Fof_anno { Ast.af_formula } ->
        begin match af_formula with
          | Ast.Sequent (xs, ys) ->
              List.iter proc_formula xs;
              List.iter proc_formula ys
          | Ast.Formula x -> proc_formula x
        end
    | Ast.Cnf_anno { Ast.af_formula = Ast.Clause lits } ->
        List.iter proc_lit lits
    | Ast.Include _ | Ast.Comment _ -> ()

(* By default new function symbols are auxiliary. *)
let func_of_tptp ?(aux = true) symdb smap func =
  try
    let id = Hashtbl.find smap.of_tptp func in
    if Symb.kind id = Symb.Func then
      id
    else
      failwith "func_of_tptp: Symbol already used as predicate"
  with
    | Not_found ->
        let id =
          match func with
            | Atomic_word (_, arity) -> Symb.add_func symdb arity
            | Number _ | String _ ->
                let id = Symb.add_func symdb 0 in
                Symb.set_distinct_constant symdb id true;
                id in
        Symb.set_auxiliary symdb id aux;
        add_to_smap smap func id;
        id

(* By default new predicate symbols are auxiliary. *)
let pred_of_tptp ?(aux = true) symdb smap pred =
  try
    let id = Hashtbl.find smap.of_tptp pred in
    if Symb.kind id = Symb.Pred then
      id
    else
      failwith "pred_of_tptp: Symbol already used as function"
  with
    | Not_found ->
        let id =
          match pred with
            | Atomic_word (_, arity) -> Symb.add_pred symdb arity
            | Number _ | String _ ->
                failwith "pred_of_tptp: Constant used as predicate" in
        Symb.set_auxiliary symdb id aux;
        add_to_smap smap pred id;
        id

let term_of_tptp tfunc tvar =
  let rec transl = function
    | Ast.Var x -> T.var (tvar x)
    | Ast.Func (func, args) ->
        let arity = List.length args in
        let id = tfunc (Atomic_word (func, arity)) in
        Term.func (id, Earray.of_list (BatList.map transl args))
    | Ast.Number n ->
        let id = tfunc (Number n) in
        Term.func (id, Earray.empty)
    | Ast.String s ->
        let id = tfunc (String s) in
        Term.func (id, Earray.empty) in
  transl

let atom_of_tptp tpred tfunc tvar = function
  | Ast.Equals (l, r) ->
      let l = term_of_tptp tfunc tvar l in
      let r = term_of_tptp tfunc tvar r in
      (S.sym_eq, Earray.of_list [ l; r ])
  | Ast.Pred (pred, args) ->
      let arity = List.length args in
      let id = tpred (Atomic_word (pred, arity)) in
      (id, Earray.of_list (BatList.map (term_of_tptp tfunc tvar) args))

let lit_of_tptp tpred tfunc tvar (Ast.Lit (sign, atom)) =
  let sign =
    match sign with
      | Ast.Pos -> Sh.Pos
      | Ast.Neg -> Sh.Neg in
  let (pred, args) = atom_of_tptp tpred tfunc tvar atom in
  L.lit (sign, pred, args)

let clause_of_tptp tpred tfunc (Ast.Clause lits) =
  let vars = Hashtbl.create 20 in
  let tvar x =
    if Hashtbl.mem vars x then
      Hashtbl.find vars x
    else
      let v = Hashtbl.length vars in
      let _ = Hashtbl.add vars x v in
      v in
  BatList.map (lit_of_tptp tpred tfunc tvar) lits

let check_role role =
  let open Ast in
  match role with
    | R_axiom
    | R_hypothesis
    | R_definition
    | R_lemma
    | R_theorem
    | R_corollary
    | R_negated_conjecture
    | R_plain
    (* [R_fi_domain], [R_fi_functors], [R_fi_predicates] are allowed
       so finite models can be checked.
    *)
    | R_fi_domain
    | R_fi_functors
    | R_fi_predicates -> ()
    | R_assumption
    (* [R_conjecture] is not allowed in CNF. *)
    | R_conjecture
    | R_type
    | R_unknown -> failwith "check_role: Unsupported role"

(* New symbols will be auxiliary. *)
let clauses_of_tptp symdb smap inputs : Clause.t list =
  let tpred = pred_of_tptp symdb smap in
  let tfunc = func_of_tptp symdb smap in
  BatList.filter_map
    (function
      | Ast.Fof_anno _ -> failwith "clauses_of_tptp: Unexpected FOF formula"
      | Ast.Cnf_anno { Ast.af_role; Ast.af_formula } ->
          check_role af_role;
          Some (clause_of_tptp tpred tfunc af_formula)
      | Ast.Include _ -> failwith "clauses_of_tptp: Unexpected include"
      | Ast.Comment _ -> None)
    inputs

let is_fof_formula = function
  | Ast.Fof_anno _ -> true
  | Ast.Cnf_anno _ | Ast.Include _ | Ast.Comment _ -> false

let is_conjecture = function
  | Ast.Fof_anno { Ast.af_role }
  | Ast.Cnf_anno { Ast.af_role } -> af_role = Ast.R_conjecture
  | Ast.Include _ | Ast.Comment _ -> false

let prob_of_tptp clausify inputs =
  let needs_clausification = List.exists is_fof_formula inputs in
  let has_conjecture = List.exists is_conjecture inputs in

  let prob = Prob.create () in
  let symdb = prob.Prob.symbols in
  let smap = {
    of_tptp = Hashtbl.create 20;
    to_tptp = Hashtbl.create 20;
  } in

  (* Read symbols from the original input. These won't be auxiliary. *)
  let add_pred pred = pred_of_tptp ~aux:false symdb smap pred |> ignore in
  let add_func func = func_of_tptp ~aux:false symdb smap func |> ignore in
  List.iter
    (iter_symbols add_pred add_func)
    inputs;

  (* Clausification. *)
  let inputs =
    if needs_clausification
    then clausify inputs
    else inputs in

  (* Read clauses. Symbols introduced by clausification will be auxiliary. *)
  let add_clause lits =
    BatDynArray.add prob.Prob.clauses
      { Clause2.cl_id = Prob.fresh_id prob; Clause2.cl_lits = lits } in
  inputs
  |> clauses_of_tptp symdb smap
  |> List.iter add_clause;

  {
    smap;
    prob;
    has_conjecture;
  }

let of_file clausify base_dir file =
  let inputs = Tptp.File.read ~base_dir file in
  prob_of_tptp clausify inputs

(* Translate variable names. *)
let var_to_tptp =
  let names =
    BatChar.range ~until:'Z' 'A'
    |> BatEnum.map (BatString.make 1)
    |> BatEnum.map Ast.to_var
    |> Earray.of_enum in
  fun x ->
    if x >= Earray.length names then
      Ast.to_var (Printf.sprintf "X%i" x)
    else if x < 0 then
      Ast.to_var (Printf.sprintf "Y%i" ~-x)
    else
      names.(x)

(* Creates a generator for integers.

   Used when generating new TPTP symbols.
*)
let make_int_gen () =
  let last = ref 0 in
  fun () ->
    incr last;
    !last

(* Translates symbol to TPTP symbol.

   If no corresponding TPTP symbol exists in [smap]
   then a new TPTP symbol will be created and added to [smap].
*)
let symb_to_tptp gen_int symdb smap id =
  let rec fresh_symb distinct_const =
    let s = Printf.sprintf "z%d" (gen_int ()) in
    let symb =
      if distinct_const then
        String (Ast.to_tptp_string s)
      else
        Atomic_word (Ast.Plain_word (Ast.to_plain_word s), Symb.arity id) in
    if Hashtbl.mem smap.of_tptp symb
    then fresh_symb distinct_const
    else symb in

  try Hashtbl.find smap.to_tptp id
  with
    | Not_found ->
        let symb = fresh_symb (S.distinct_constant symdb id) in
        add_to_smap smap symb id;
        symb

let term_to_tptp tfunc tvar =
  let rec transl = function
    | T.Var x -> Ast.Var (tvar x)
    | T.Func (s, args) ->
        match tfunc s with
          | Atomic_word (symb, _) ->
              let args =
                args
                |> Earray.map transl
                |> Earray.to_list in
              Ast.Func (symb, args)
          | Number q -> Ast.Number q
          | String s -> Ast.String s in
  transl

let atom_to_tptp tpred tfunc tvar (s, args) =
  match%earr args with
    | [| l; r |] when s = S.sym_eq ->
        let l = term_to_tptp tfunc tvar l in
        let r = term_to_tptp tfunc tvar r in
        Ast.Equals (l, r)
    | _ ->
        match tpred s with
          | Atomic_word (symb, _) ->
              let args =
                args
                |> Earray.map (term_to_tptp tfunc tvar)
                |> Earray.to_list in
              Ast.Pred (symb, args)
          | Number _
          | String _ -> failwith "atom_to_tptp: Constant used as predicate"

let lit_to_tptp tpred tfunc tvar (L.Lit (sign, s, args)) =
  let sign =
    match sign with
      | Sh.Pos -> Ast.Pos
      | Sh.Neg -> Ast.Neg in
  Ast.Lit (sign, atom_to_tptp tpred tfunc tvar (s, args))

let clause_to_tptp tpred tfunc lits =
  Ast.Clause (BatList.map (lit_to_tptp tpred tfunc var_to_tptp) lits)

type commutativity =
  | Export
  | Export_flat

(* Generates clauses which depict commutativity of [s]. *)
let symb_commutativity_clauses symdb format s =
  if S.commutative symdb s then
    let arity = Symb.arity s in
    let args = BatList.init arity T.var in
    let args' =
      (* Swap first two arguments. *)
      match args with
        | a :: b :: rest -> b :: a :: rest
        | _ -> failwith "symb_commutativity_clauses" in
    match S.kind s with
      | S.Func ->
          let l = T.func (s, Earray.of_list args) in
          let r = T.func (s, Earray.of_list args') in
          [
            if format = Export_flat
            then [ L.mk_eq (T.var arity) l; L.mk_ineq (T.var arity) r ]
            else [ L.mk_eq l r ]
          ]
      | S.Pred ->
          let u = [
            L.lit (Sh.Pos, s, Earray.of_list args);
            L.lit (Sh.Neg, s, Earray.of_list args');
          ] in
          let v = BatList.map Lit.neg u in
          [ u; v ]
  else
    []


let tptp_clause_to_input ?(name = "cl") clause =
  Ast.Cnf_anno {
    Ast.af_name = Ast.N_word (Ast.to_plain_word name);
    Ast.af_role = Ast.R_axiom;
    Ast.af_formula = clause;
    Ast.af_annos = None;
  }

let prob_to_tptp tp comm f =
  let clauses = tp.prob.Prob.clauses in

  let seen_symbs = ref Symb.Set.empty in

  let gen_int = make_int_gen () in
  let tsymb id =
    seen_symbs := Symb.Set.add id !seen_symbs;
    symb_to_tptp gen_int tp.prob.Prob.symbols tp.smap id in
  let proc_clause lits =
    lits
    |> clause_to_tptp tsymb tsymb
    |> tptp_clause_to_input
    |> f in

  BatDynArray.iter (fun cl2 -> proc_clause cl2.Clause2.cl_lits) clauses;

  (* Clauses for commutative symbols. *)
  Symb.Set.iter
    (fun s ->
      let cls = symb_commutativity_clauses tp.prob.Prob.symbols comm s in
      List.iter proc_clause cls)
    !seen_symbs;

  !seen_symbs

let clause_to_string symdb smap lits =
  let gen_int = make_int_gen () in
  let tsymb = symb_to_tptp gen_int symdb smap in
  lits
  |> clause_to_tptp tsymb tsymb
  |> tptp_clause_to_input
  |> Tptp.to_string

module M = Model

let model_to_tptp
    ?prob_name
    ?(role_dom = Ast.R_fi_domain)
    ?(role_pred = Ast.R_fi_predicates)
    ?(role_func = Ast.R_fi_functors)
    p
    model
    interp_name
    f =

  let symb_to_tptp =
    let gen_int = make_int_gen () in
    symb_to_tptp gen_int p.prob.Prob.symbols p.smap in

  (* All distinct constants. *)
  let distinct_consts =
    p.prob.Prob.symbols
    |> Symb.distinct_consts
    |> Symb.Set.enum
    |> BatList.of_enum
    |> BatList.map (fun id -> symb_to_tptp id, id) in

  (* Maps domain elements to TPTP symbols. *)
  let dom_to_tptp =
    let dom_to_tptp = Earray.make model.M.max_size None in
    (* Numbers used by distinct constants. *)
    let used_nums = Hashtbl.create 20 in

    (* Assure that distinct constants are interpreted as themselves. *)
    List.iter
      (fun (tptp_symb, s) ->
        try
          match tptp_symb with
            | Atomic_word (_, _) ->
                failwith "distinct constant mapped to atomic word"
            | Number n ->
                let v = (Symb.Map.find s model.M.symbs).M.values.(0) in
                dom_to_tptp.(v) <- Some tptp_symb;
                Hashtbl.add used_nums n ()
            | String _ ->
                let v = (Symb.Map.find s model.M.symbs).M.values.(0) in
                dom_to_tptp.(v) <- Some tptp_symb
        with
          | Not_found -> failwith "distinct constant not in model")
      distinct_consts;

    (* Assign unused numbers to remaining domain elements. *)
    let last_num = ref ~-1 in
    Earray.iteri
      (fun i -> function
      | None ->
          incr last_num;
          while Hashtbl.mem used_nums (Q.of_int !last_num) do
            incr last_num
          done;
          dom_to_tptp.(i) <- Some (Number (Q.of_int !last_num))
      | Some _ -> ())
      dom_to_tptp;

    Earray.map
      (function
      | None
      | Some (Atomic_word _) ->
          failwith "element not mapped to distinct constant"
      | Some (Number n) -> Ast.Number n
      | Some (String str) -> Ast.String str)
      dom_to_tptp in

  (* Comment - start of finite model. *)
  let for_prob =
    match prob_name with
      | None -> ""
      | Some n -> " for " ^ n in
  f (Ast.Comment (Ast.to_comment_line
                    (" SZS output start FiniteModel" ^ for_prob)));

  (* Comment with domain size. *)
  f (Ast.Comment (Ast.to_comment_line
                    (" domain size: " ^ string_of_int model.Model.max_size)));

  (* Specify domain - one formula. *)
  let _ =
    let x = Ast.to_var "X" in
    let atoms =
      let x = Ast.Var x in
      Earray.map (fun el -> Ast.Atom (Ast.Equals (x, el))) dom_to_tptp in
    let disjunction =
      Earray.reduce (fun a b -> Ast.Binop (Ast.Or, a, b)) atoms in
    let formula = Ast.Formula (Ast.Quant (Ast.All, x, disjunction)) in
    f
      (Ast.Fof_anno {
        Ast.af_name = interp_name;
        Ast.af_role = role_dom;
        Ast.af_formula = formula;
        Ast.af_annos = None;
      }) in

  (* Symbols from [model]. The symbols are sorted
     to ensure that TPTP model generation is deterministic.
  *)
  let sorted_symbs =
    model.Model.symbs
    |> Symb.Map.enum
    |> BatList.of_enum
    |> BatList.map (fun (id, _) -> symb_to_tptp id, id)
    |> BatList.sort compare in

  (* Interpretation of symbols - one formula for each symbol. *)
  List.iter
    (fun (tptp_symb, s) ->
      match tptp_symb with
        | Atomic_word (w, arity) ->
            let role, atoms =
              let param_sizes = Earray.make arity model.M.max_size in
              let values = (Symb.Map.find s model.M.symbs).M.values in
              let a = Earray.make arity ~-1 in
              let i = ref 0 in
              let atoms = BatDynArray.make (Earray.length values) in
              if Symb.kind s == Symb.Pred then begin
                Assignment.each a 0 arity param_sizes model.M.max_size
                  (fun a ->
                    let args =
                      BatList.init arity (fun j -> dom_to_tptp.(a.(j))) in
                    let atom = Ast.Atom (Ast.Pred (w, args)) in
                    begin match values.(!i) with
                      | 0 -> BatDynArray.add atoms (Ast.Not atom)
                      | 1 -> BatDynArray.add atoms atom
                      | _ -> failwith "invalid value of predicate"
                    end;
                    incr i);
                role_pred, atoms
              end else begin
                Assignment.each a 0 arity param_sizes model.M.max_size
                  (fun a ->
                    let args =
                      BatList.init arity (fun j -> dom_to_tptp.(a.(j))) in
                    let cell = Ast.Func (w, args) in
                    let v = dom_to_tptp.(values.(!i)) in
                    let atom = Ast.Atom (Ast.Equals (cell, v)) in
                    BatDynArray.add atoms atom;
                    incr i);
                role_func, atoms
              end in
            let conjunction =
              let f = ref (BatDynArray.get atoms 0) in
              for i = 1 to BatDynArray.length atoms - 1 do
                f := Ast.Binop (Ast.And, !f, BatDynArray.get atoms i);
              done;
              !f in
            f
              (Ast.Fof_anno {
                Ast.af_name = interp_name;
                Ast.af_role = role;
                Ast.af_formula = Ast.Formula conjunction;
                Ast.af_annos = None;
              })
        (* Skip distinct constants - they are interpreted as themselves. *)
        | Number _ | String _ -> ())
    sorted_symbs;

  (* Comment - end of finite model. *)
  f (Ast.Comment (Ast.to_comment_line
                    (" SZS output end FiniteModel" ^ for_prob)));
