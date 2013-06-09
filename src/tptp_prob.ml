(* Copyright (c) 2013 Radek Micek *)

open BatPervasives

module S = Symb
module L = Lit
module Ast = Tptp_ast

type tptp_symbol =
  | Atomic_word of Ast.atomic_word * Symb.arity
  | Number of Q.t
  | String of Ast.tptp_string

type 's symb_map = {
  of_tptp : (tptp_symbol, 's Symb.id) Hashtbl.t;
  to_tptp : ('s Symb.id, tptp_symbol) Hashtbl.t;
}

type 's t = {
  smap : 's symb_map;
  preds : (tptp_symbol, bool) Hashtbl.t;
  prob : 's Prob.t;
}

type wt =
  | Wr : 's t -> wt

let add_clause p (Ast.Clause lits) =
  let vars = Hashtbl.create 20 in

  let add_func = Symb.add_func p.prob.Prob.symbols in
  let add_pred = Symb.add_pred p.prob.Prob.symbols in

  let rec transl_term = function
    | Ast.Var x ->
        let v =
          if Hashtbl.mem vars x then
            Hashtbl.find vars x
          else
            let v = Hashtbl.length vars in
            let _ = Hashtbl.add vars x v in
            v in
        Term.var v
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
            let id = add_func arity in
            let _ = Hashtbl.add p.smap.of_tptp s id in
            let _ = Hashtbl.add p.smap.to_tptp id s in
            let _ = Hashtbl.add p.preds s false in
            id in
        Term.func (id, Array.of_list (BatList.map transl_term args))
    | Ast.Number n ->
        let s = Number n in
        let id =
          if Hashtbl.mem p.smap.of_tptp s then
            Hashtbl.find p.smap.of_tptp s
          else
            let id = add_func 0 in
            let _ = Hashtbl.add p.smap.of_tptp s id in
            let _ = Hashtbl.add p.smap.to_tptp id s in
            let _ = BatDynArray.add p.prob.Prob.distinct_consts id in
            id in
        Term.func (id, [| |])
    | Ast.String s ->
        let s = String s in
        let id =
          if Hashtbl.mem p.smap.of_tptp s then
            Hashtbl.find p.smap.of_tptp s
          else
            let id = add_func 0 in
            let _ = Hashtbl.add p.smap.of_tptp s id in
            let _ = Hashtbl.add p.smap.to_tptp id s in
            let _ = BatDynArray.add p.prob.Prob.distinct_consts id in
            id in
        Term.func (id, [| |]) in

  let transl_sign = function
    | Ast.Pos -> Sh.Pos
    | Ast.Neg -> Sh.Neg in

  let transl_lit = function
    | Ast.Lit (sign, Ast.Equals (l, r)) ->
        let l = transl_term l in
        let r = transl_term r in
        L.lit (transl_sign sign, S.sym_eq, [| l; r |])
    | Ast.Lit (sign, Ast.Pred (pred, args)) ->
        let sign = transl_sign sign in
        let arity = List.length args in
        let s = Atomic_word (pred, arity) in
        let id =
          if Hashtbl.mem p.smap.of_tptp s then
            if not (Hashtbl.find p.preds s) then
              failwith "Symbol is already used as a function"
            else
              Hashtbl.find p.smap.of_tptp s
          else
            let id = add_pred arity in
            let _ = Hashtbl.add p.smap.of_tptp s id in
            let _ = Hashtbl.add p.smap.to_tptp id s in
            let _ = Hashtbl.add p.preds s true in
            id in
        L.lit (sign, id, Array.of_list (BatList.map transl_term args)) in

  let clause = {
    Clause2.cl_id = Prob.fresh_id p.prob;
    Clause2.cl_lits = BatList.map transl_lit lits;
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
  let a = BatPathGen.OfString.of_string a in
  let b = BatPathGen.OfString.of_string b in
  let res =
    if BatPathGen.OfString.is_absolute b
    then b
    else (BatPathGen.OfString.concat a b) in
  BatPathGen.OfString.to_ustring
    (BatPathGen.OfString.normalize_in_tree res)

let of_file base_dir file =

  let Prob.Wr prob = Prob.create () in
  let p = {
    smap = { of_tptp = Hashtbl.create 20; to_tptp = Hashtbl.create 20 };
    preds = Hashtbl.create 20;
    prob;
  } in

  let rec of_file file selected =
    BatFile.with_file_in file (fun i ->
      let lexbuf = BatLexing.from_input i in
      let proc_clause name cl =
        if selected name then
          add_clause p cl in
      let proc_include (file : Ast.file_name) sel =
        let path = combine_paths base_dir (file :> string) in
        let selected' =
          if sel = [] then
            selected
          else
            (fun name ->
              selected name && Elist.contains name sel) in
        of_file path selected' in
      with_dispose
        ~dispose:Tptp.close_in
        (iter_tptp_input proc_clause proc_include)
        (Tptp.create_in lexbuf)) in

  of_file file (fun _ -> true);
  Wr p

module M = Model

let model_to_tptp
    ?(role_dom = Ast.R_fi_domain)
    ?(role_pred = Ast.R_fi_predicates)
    ?(role_func = Ast.R_fi_functors)
    p
    model
    interp_name
    f =

  (* Maps doamin elements to TPTP symbols. *)
  let dom_to_tptp =
    let dom_to_tptp = Array.make model.M.max_size None in
    (* Numbers used by distinct constants. *)
    let used_nums = Hashtbl.create 20 in

    (* Assure that distinct constants are interpreted as themselves. *)
    Hashtbl.iter
      (fun tptp_symb s ->
        try
          match tptp_symb with
            | Atomic_word (_, _) -> ()
            | Number n ->
                let v = (Symb.Map.find s model.M.symbs).M.values.(0) in
                dom_to_tptp.(v) <- Some tptp_symb;
                Hashtbl.add used_nums n ()
            | String _ ->
                let v = (Symb.Map.find s model.M.symbs).M.values.(0) in
                dom_to_tptp.(v) <- Some tptp_symb
        with
          | Not_found -> failwith "distinct constant not in model")
      p.smap.of_tptp;

    (* Assign unused numbers to remaining domain elements. *)
    let last_num = ref ~-1 in
    Array.iteri
      (fun i -> function
      | None ->
          incr last_num;
          while Hashtbl.mem used_nums (Q.of_int !last_num) do
            incr last_num
          done;
          dom_to_tptp.(i) <- Some (Number (Q.of_int !last_num))
      | Some _ -> ())
      dom_to_tptp;

    Array.map
      (function
      | None
      | Some (Atomic_word _) ->
          failwith "element not mapped to distinct constant"
      | Some (Number n) -> Ast.Number n
      | Some (String str) -> Ast.String str)
      dom_to_tptp in

  (* Comment with domain size. *)
  f (Ast.Comment (Ast.to_comment_line
                    (" domain size: " ^ string_of_int model.Model.max_size)));

  (* Specify domain - one formula. *)
  let _ =
    let x = Ast.to_var "X" in
    let atoms =
      let x = Ast.Var x in
      Array.map (fun el -> Ast.Atom (Ast.Equals (x, el))) dom_to_tptp in
    let disjunction =
      BatArray.reduce (fun a b -> Ast.Binop (Ast.Or, a, b)) atoms in
    let formula = Ast.Formula (Ast.Quant (Ast.All, x, disjunction)) in
    f
      (Ast.Fof_anno {
        Ast.af_name = interp_name;
        Ast.af_role = role_dom;
        Ast.af_formula = formula;
        Ast.af_annos = None;
      }) in

  (* Interpretation of symbols - one formula for each symbol. *)
  Hashtbl.iter
    (fun tptp_symb s ->
      match tptp_symb with
        | Atomic_word (w, arity) ->
            let role, atoms =
              let param_sizes = Array.make arity model.M.max_size in
              let values = (Symb.Map.find s model.M.symbs).M.values in
              let a = Array.make arity ~-1 in
              let i = ref 0 in
              let atoms = BatDynArray.make (Array.length values) in
              if Hashtbl.find p.preds tptp_symb then begin
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
    p.smap.of_tptp
