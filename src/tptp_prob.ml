(* Copyright (c) 2013 Radek Micek *)

open BatPervasives

module S = Symb
module T = Term
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

let (|>) = BatPervasives.(|>)

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
          | Ast.R_negated_conjecture
          | Ast.R_plain ->
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

let of_file ?(prob = None) base_dir file =

  let Wr p =
    match prob with
      | Some p -> p
      | None ->
          let Prob.Wr prob = Prob.create () in
          Wr {
            smap = {
              of_tptp = Hashtbl.create 20;
              to_tptp = Hashtbl.create 20
            };
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

type commutativity =
  | Ignore
  | Export
  | Export_flat

let prob_to_tptp tp comm f =
  let clauses = tp.prob.Prob.clauses in

  (* Translate variable names. *)
  let var =
    let names =
      BatChar.range ~until:'Z' 'A'
      |> BatEnum.map (BatString.make 1)
      |> BatEnum.map Ast.to_var
      |> BatArray.of_enum in
    fun x ->
      if x >= Array.length names then
        Ast.to_var (Printf.sprintf "X%i" x)
      else if x < 0 then
        Ast.to_var (Printf.sprintf "Y%i" ~-x)
      else
        names.(x) in

  (* Return name for auxiliary symbol. *)
  let aux_symb =
    let last = ref 0 in
    let rec gen_symb s =
      incr last;
      let symb =
        Ast.Plain_word (Ast.to_plain_word (Printf.sprintf "z%d" !last)) in
      if Hashtbl.mem tp.smap.of_tptp (Atomic_word (symb, S.arity s))
      then gen_symb s
      else symb in
    let symbs = Hashtbl.create 20 in
    fun s ->
      try Hashtbl.find symbs s
      with Not_found ->
        let symb = gen_symb s in
        Hashtbl.add symbs s symb;
        symb in

  let seen_funcs = ref BatSet.empty in

  let rec transl_term = function
    | T.Var x -> Ast.Var (var x)
    | T.Func (s, args) ->
        seen_funcs := BatSet.add s !seen_funcs;
        let args () =
          args
          |> Array.map transl_term
          |> Array.to_list in
        try
          match Hashtbl.find tp.smap.to_tptp s with
            | Atomic_word (symb, _) ->
                Ast.Func (symb, args ())
            | Number q -> Ast.Number q
            | String s -> Ast.String s
        with
          | Not_found ->
              let symb = aux_symb s in
              Ast.Func (symb, args ()) in

  let transl_lit (L.Lit (sign, s, args)) =
    let atom =
      match args with
        | [| l; r |] when s = S.sym_eq ->
            let l' = transl_term l in
            let r' = transl_term r in
            Ast.Equals (l', r')
        | _ ->
            let args () =
              args
              |> Array.map transl_term
              |> Array.to_list in
            try
              match Hashtbl.find tp.smap.to_tptp s with
                | Number _
                | String _ -> failwith "problem_to_tptp"
                | Atomic_word (symb, _) ->
                    Ast.Pred (symb, args ())
            with
              | Not_found ->
                  let symb = aux_symb s in
                  Ast.Pred (symb, args ()) in
    let sign =
      match sign with
        | Sh.Pos -> Ast.Pos
        | Sh.Neg -> Ast.Neg in
    Ast.Lit (sign, atom) in

  let make_cnf lits =
    Ast.Cnf_anno {
      Ast.af_name = Ast.N_word (Ast.to_plain_word "cl");
      Ast.af_role = Ast.R_axiom;
      Ast.af_formula = Ast.Clause lits;
      Ast.af_annos = None;
    } in

  let proc_clause cl =
    let lits =
      cl.Clause2.cl_lits
      |> BatList.map transl_lit in
    f (make_cnf lits) in

  BatDynArray.iter proc_clause clauses;

  (* Generate commutativity clauses for seen commutative function symbols. *)
  BatSet.iter
    (fun s ->
      if S.commutative tp.prob.Prob.symbols s then begin
        let arity = Symb.arity s in
        let args = BatList.init arity (fun i -> Ast.Var (var i)) in
        let args' =
          (* Swap first two arguments. *)
          match args with
            | a :: b :: rest -> b :: a :: rest
            | _ -> failwith "problem_to_tptp" in
        let lits symb =
          let l = Ast.Func (symb, args) in
          let r = Ast.Func (symb, args') in
          if comm = Export_flat then
            [
              Ast.Lit (Ast.Pos, Ast.Equals (Ast.Var (var arity), l));
              Ast.Lit (Ast.Neg, Ast.Equals (Ast.Var (var arity), r));
            ]
          else
            [ Ast.Lit (Ast.Pos, Ast.Equals (l, r)) ] in
        let lits =
          try
            match Hashtbl.find tp.smap.to_tptp s with
              | Number _
              | String _ -> failwith "problem_to_tptp"
              | Atomic_word (symb, _) -> lits symb
          with
            | Not_found -> lits (aux_symb s) in
        f (make_cnf lits)
       end)
    (if comm <> Ignore then !seen_funcs else BatSet.empty)

module M = Model

let model_to_tptp
    ?(role_dom = Ast.R_fi_domain)
    ?(role_pred = Ast.R_fi_predicates)
    ?(role_func = Ast.R_fi_functors)
    p
    model
    interp_name
    f =

  (* Sort symbols to make model generation deterministic. *)
  let sorted_symbs =
    p.smap.of_tptp
    |> BatHashtbl.enum
    |> BatList.of_enum
    |> BatList.sort compare in

  (* Maps doamin elements to TPTP symbols. *)
  let dom_to_tptp =
    let dom_to_tptp = Array.make model.M.max_size None in
    (* Numbers used by distinct constants. *)
    let used_nums = Hashtbl.create 20 in

    (* Assure that distinct constants are interpreted as themselves. *)
    List.iter
      (fun (tptp_symb, s) ->
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
      sorted_symbs;

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
  List.iter
    (fun (tptp_symb, s) ->
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
    sorted_symbs
