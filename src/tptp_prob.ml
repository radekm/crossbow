(* Copyright (c) 2013 Radek Micek *)

open BatPervasives

module Ast = Tptp_ast

type tptp_symbol =
  | Atomic_word of Ast.atomic_word * Symb.arity
  | Number of Q.t
  | String of Ast.tptp_string

type symb_map = {
  of_tptp : (tptp_symbol, Symb.id) Hashtbl.t;
  to_tptp : (Symb.id, tptp_symbol) Hashtbl.t;
}

type t = {
  smap : symb_map;
  preds : (tptp_symbol, bool) Hashtbl.t;
  prob : Prob.t;
}

let add_clause p (Ast.Clause lits) =
  let vars = Hashtbl.create 20 in

  let create_symb = function
    | Atomic_word (_, arity) -> Symb.add_anon_symb p.prob.Prob.symbols arity
    | Number _
    | String _ -> Symb.add_anon_symb p.prob.Prob.symbols 0 in

  let rec transl_term = function
    | Ast.Var x ->
        let v =
          if Hashtbl.mem vars x then
            Hashtbl.find vars x
          else
            let v = Hashtbl.length vars in
            let _ = Hashtbl.add vars x v in
            v in
        Term.Var v
    | Ast.Func (func, args) ->
        let arity = List.length args in
        let s = Atomic_word (func, arity) in
        let id =
          if Hashtbl.mem p.smap.of_tptp s then
            if Hashtbl.find p.preds s then
              failwith "Symbol is already used as a predicate"
            else
              Hashtbl.find p.smap.of_tptp s
          else
            let id = create_symb s in
            let _ = Hashtbl.add p.smap.of_tptp s id in
            let _ = Hashtbl.add p.smap.to_tptp id s in
            let _ = Hashtbl.add p.preds s false in
            id in
        Term.Func (id, Array.of_list (BatList.map transl_term args))
    | Ast.Number n ->
        let s = Number n in
        let id =
          if Hashtbl.mem p.smap.of_tptp s then
            Hashtbl.find p.smap.of_tptp s
          else
            let id = create_symb s in
            let _ = Hashtbl.add p.smap.of_tptp s id in
            let _ = Hashtbl.add p.smap.to_tptp id s in
            let _ = BatDynArray.add p.prob.Prob.distinct_consts id in
            id in
        Term.Func (id, [| |])
    | Ast.String s ->
        let s = String s in
        let id =
          if Hashtbl.mem p.smap.of_tptp s then
            Hashtbl.find p.smap.of_tptp s
          else
            let id = create_symb s in
            let _ = Hashtbl.add p.smap.of_tptp s id in
            let _ = Hashtbl.add p.smap.to_tptp id s in
            let _ = BatDynArray.add p.prob.Prob.distinct_consts id in
            id in
        Term.Func (id, [| |]) in

  let transl_atom = function
    | Ast.Equals (l, r) ->
        let l = transl_term l in
        let r = transl_term r in
        Term.mk_eq l r
    | Ast.Pred (pred, args) ->
        let arity = List.length args in
        let s = Atomic_word (pred, arity) in
        let id =
          if Hashtbl.mem p.smap.of_tptp s then
            if not (Hashtbl.find p.preds s) then
              failwith "Symbol is already used as a function"
            else
              Hashtbl.find p.smap.of_tptp s
          else
            let id = create_symb s in
            let _ = Hashtbl.add p.smap.of_tptp s id in
            let _ = Hashtbl.add p.smap.to_tptp id s in
            let _ = Hashtbl.add p.preds s true in
            id in
        Term.Func (id, Array.of_list (BatList.map transl_term args)) in

  let transl_lit = function
    | Ast.Lit (Ast.Pos, atom) -> transl_atom atom
    | Ast.Lit (Ast.Neg, atom) -> Clause.neg_lit (transl_atom atom) in

  let clause = {
    Clause.cl_id = Prob.fresh_id p.prob;
    Clause.cl_lits = BatList.map transl_lit lits;
  } in

  BatDynArray.add p.prob.Prob.clauses clause

let rec iter_tptp_input
    (proc_clause : Ast.formula_name -> Ast.cnf_formula -> unit)
    (proc_include : Ast.file_name -> Ast.formula_name list -> unit)
    (input : Tptp.input) : unit =

  match Tptp.read input with
    | None -> ()
    | Some (Ast.Fof_anno _) -> failwith "Unexpected fof"
    | Some (Ast.Cnf_anno ca) ->
        begin match ca.Tptp_ast.af_role with
          | Ast.R_axiom
          | Ast.R_hypothesis
          | Ast.R_definition
          | Ast.R_lemma
          | Ast.R_theorem
          | Ast.R_negated_conjecture ->
              proc_clause ca.Tptp_ast.af_name ca.Tptp_ast.af_formula;
              iter_tptp_input proc_clause proc_include input
          | _ -> failwith "Unexpected role"
        end
    | Some (Ast.Include (file_name, formula_selection)) ->
        proc_include file_name formula_selection;
        iter_tptp_input proc_clause proc_include input
    | Some (Ast.Comment _) ->
        iter_tptp_input proc_clause proc_include input

let combine_paths (a : string) (b : string) : string =
  if
    a = "" ||
    b = "" ||
    BatString.ends_with a "/" ||
    BatString.starts_with b "/"
  then
    a ^ b
  else
    a ^ "/" ^ b

let of_file base_dir file =

  let p = {
    smap = { of_tptp = Hashtbl.create 20; to_tptp = Hashtbl.create 20 };
    preds = Hashtbl.create 20;
    prob = Prob.create ();
  } in

  let rec of_file file sel =
    let path = combine_paths base_dir file in
    BatFile.with_file_in path (fun i ->
      let lexbuf = BatLexing.from_input i in
      let proc_clause name cl =
        if sel = [] || List.exists (fun x -> x = name) sel then
          add_clause p cl in
      let proc_include (file : Ast.file_name) sel =
        of_file (file :> string) sel in
      with_dispose
        ~dispose:Tptp.close_in
        (iter_tptp_input proc_clause proc_include)
        (Tptp.create_in lexbuf)) in

  of_file file [];
  p
