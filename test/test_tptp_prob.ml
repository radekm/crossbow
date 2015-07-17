(* Copyright (c) 2013, 2015 Radek Micek *)

open OUnit

module Ast = Tptp_ast
module TP = Tptp_prob
module T = Term
module L = Lit
module C = Clause2

let assert_seen_symbs expected set =
  let expSet =
    expected
    |> BatList.enum
    |> Symb.Set.of_enum in
  assert_equal ~cmp:Symb.Set.equal expSet set

let base_dir = "test/data/tptp_prob/"

let clausify _ = failwith "No clausification"

let distinct_consts tptp_prob =
  tptp_prob.TP.prob.Prob.symbols
  |> Symb.distinct_consts
  |> Symb.Set.enum
  |> BatList.of_enum

let test_basic () =
  let p =
    Tptp_prob.of_file clausify base_dir (base_dir ^ "01_test_basic.p") in

  let q = Ast.Plain_word (Ast.to_plain_word "q") in
  let c = Ast.Plain_word (Ast.to_plain_word "c") in
  let d = Ast.Plain_word (Ast.to_plain_word "d") in
  let f = Ast.Plain_word (Ast.to_plain_word "f") in

  (* TPTP symbols. *)
  let q2 = TP.Atomic_word (q, 2) in
  let q1 = TP.Atomic_word (q, 1) in
  let c0 = TP.Atomic_word (c, 0) in
  let d0 = TP.Atomic_word (d, 0) in
  let f2 = TP.Atomic_word (f, 2) in
  let f1 = TP.Atomic_word (f, 1) in
  let str_hi = TP.String (Ast.to_tptp_string "hi") in
  let str_hello_world = TP.String (Ast.to_tptp_string "hello world") in
  let num_twelve_point_five = TP.Number (Q.of_ints 25 2) in
  let num_seven = TP.Number (Q.of_int 7) in

  (* All TPTP symbols are mapped. *)
  assert_equal 10 (Hashtbl.length p.TP.smap.TP.of_tptp);
  assert_equal 10 (Hashtbl.length p.TP.smap.TP.to_tptp);

  (* Find ids of TPTP symbols. *)
  let find = Hashtbl.find p.TP.smap.TP.of_tptp in
  let q2' = find q2 in
  let q1' = find q1 in
  let c0' = find c0 in
  let d0' = find d0 in
  let f2' = find f2 in
  let f1' = find f1 in
  let str_hi' = find str_hi in
  let str_hello_world' = find str_hello_world in
  let num_twelve_point_five' = find num_twelve_point_five in
  let num_seven' = find num_seven in

  (* Ids are different. *)
  let all_ids = [
    q2'; q1'; c0'; d0'; f2'; f1';
    str_hi'; str_hello_world'; num_twelve_point_five'; num_seven';
  ] in
  assert_equal 10 (List.length all_ids);
  assert_equal (List.length all_ids) (List.length (BatList.unique all_ids));

  (* Hashtable to_tptp is inverse of of_tptp
     (BTW this implies that ids are different).
  *)
  Hashtbl.iter
    (fun k v -> assert_equal k (Hashtbl.find p.TP.smap.TP.to_tptp v))
    p.TP.smap.TP.of_tptp;

  (* Array distinct_consts. *)
  assert_equal
    [str_hi'; num_twelve_point_five'; num_seven'; str_hello_world']
    (distinct_consts p);

  assert_equal false p.TP.has_conjecture;

  (* Clauses. *)
  let q2 a b = L.lit (Sh.Pos, q2', [| a; b |]) in
  let q1 a = L.lit (Sh.Pos, q1', [| a |]) in
  let c0 = T.func (c0', [| |]) in
  let d0 = T.func (d0', [| |]) in
  let f2 a b = T.func (f2', [| a; b |]) in
  let f1 a = T.func (f1', [| a |]) in
  let exp_clauses = [
    {
      C.cl_id = 0;
      C.cl_lits = [ q2 (T.var 0) c0 ];
    };
    {
      C.cl_id = 1;
      C.cl_lits = [ L.neg (q2 c0 d0) ];
    };
    {
      C.cl_id = 2;
      C.cl_lits = [
        L.mk_eq (T.var 0) (T.var 1);
        L.mk_ineq
          (f2 (T.var 0) (T.var 1))
          (f2 (T.var 1) (T.var 0));
      ];
    };
    {
      C.cl_id = 3;
      C.cl_lits = [
        L.neg
          (q2
             (T.func (str_hi', [| |]))
             (T.func (num_twelve_point_five', [| |])));
      ];
    };
    {
      C.cl_id = 4;
      C.cl_lits = [
        q1 (T.func (num_seven', [| |]));
        L.mk_eq
          (f1 (T.var 0))
          (T.func (str_hello_world', [| |]));
      ];
    };
  ] in
  assert_equal exp_clauses (BatDynArray.to_list p.TP.prob.Prob.clauses)

let test_include () =
  let p =
    Tptp_prob.of_file clausify base_dir (base_dir ^ "02_test_include.p") in

  let q = Ast.Plain_word (Ast.to_plain_word "q") in
  let c = Ast.Plain_word (Ast.to_plain_word "c") in
  let d = Ast.Plain_word (Ast.to_plain_word "d") in
  let g = Ast.Plain_word (Ast.to_plain_word "g") in

  (* TPTP symbols. *)
  let q1 = TP.Atomic_word (q, 1) in
  let c0 = TP.Atomic_word (c, 0) in
  let d0 = TP.Atomic_word (d, 0) in
  let g1 = TP.Atomic_word (g, 1) in
  let num_zero = TP.Number Q.zero in
  let num_one = TP.Number Q.one in

  (* All TPTP symbols are mapped. *)
  assert_equal 6 (Hashtbl.length p.TP.smap.TP.of_tptp);
  assert_equal 6 (Hashtbl.length p.TP.smap.TP.to_tptp);

  (* Find ids of TPTP symbols. *)
  let find = Hashtbl.find p.TP.smap.TP.of_tptp in
  let q1' = find q1 in
  let c0' = find c0 in
  let d0' = find d0 in
  let g1' = find g1 in
  let num_zero' = find num_zero in
  let num_one' = find num_one in

  (* Ids are different. *)
  let all_ids = [q1'; c0'; d0'; g1'; num_zero'; num_one'] in
  assert_equal 6 (List.length all_ids);
  assert_equal (List.length all_ids) (List.length (BatList.unique all_ids));

  (* Hashtable to_tptp is inverse of of_tptp
     (BTW this implies that ids are different).
  *)
  Hashtbl.iter
    (fun k v -> assert_equal k (Hashtbl.find p.TP.smap.TP.to_tptp v))
    p.TP.smap.TP.of_tptp;

  (* Array distinct_consts. *)
  assert_equal
    [num_zero'; num_one']
    (distinct_consts p);

  assert_equal false p.TP.has_conjecture;

  (* Clauses. *)
  let q1 a = L.lit (Sh.Pos, q1', [| a |]) in
  let c0 = T.func (c0', [| |]) in
  let d0 = T.func (d0', [| |]) in
  let g1 a = T.func (g1', [| a |]) in
  let num_zero = T.func (num_zero', [| |]) in
  let num_one = T.func (num_one', [| |]) in
  let exp_clauses = [
    {
      C.cl_id = 0;
      C.cl_lits = [ L.mk_eq (g1 c0) num_zero ];
    };
    {
      C.cl_id = 1;
      C.cl_lits = [ L.mk_eq c0 (g1 (g1 c0)) ];
    };
    {
      C.cl_id = 2;
      C.cl_lits = [ q1 c0 ];
    };
    {
      C.cl_id = 3;
      C.cl_lits = [ L.mk_ineq d0 c0 ];
    };
    {
      C.cl_id = 4;
      C.cl_lits = [ L.mk_eq d0 num_one ];
    };
    {
      C.cl_id = 5;
      C.cl_lits = [ q1 d0 ];
    };
  ] in
  assert_equal exp_clauses (BatDynArray.to_list p.TP.prob.Prob.clauses)

let test_nested_include () =
  let p =
    Tptp_prob.of_file clausify base_dir
      (base_dir ^ "03_test_nested_include.p") in

  let r = Ast.Plain_word (Ast.to_plain_word "r") in
  let s = Ast.Plain_word (Ast.to_plain_word "s") in

  (* TPTP symbols. *)
  let r0 = TP.Atomic_word (r, 0) in
  let s0 = TP.Atomic_word (s, 0) in

  (* All TPTP symbols are mapped. *)
  assert_equal 2 (Hashtbl.length p.TP.smap.TP.of_tptp);
  assert_equal 2 (Hashtbl.length p.TP.smap.TP.to_tptp);

  (* Find ids of TPTP symbols. *)
  let find = Hashtbl.find p.TP.smap.TP.of_tptp in
  let r0' = find r0 in
  let s0' = find s0 in

  (* Ids are different. *)
  assert_bool "" (r0' <> s0');

  (* Hashtable to_tptp is inverse of of_tptp
     (BTW this implies that ids are different).
  *)
  Hashtbl.iter
    (fun k v -> assert_equal k (Hashtbl.find p.TP.smap.TP.to_tptp v))
    p.TP.smap.TP.of_tptp;

  (* Array distinct_consts. *)
  assert_equal
    []
    (distinct_consts p);

  assert_equal false p.TP.has_conjecture;

  (* Clauses. *)
  let exp_clauses = [
    {
      C.cl_id = 0;
      C.cl_lits = [
        L.lit (Sh.Pos, r0', [| |]);
        L.neg (L.lit (Sh.Pos, s0', [| |]))
      ];
    };
  ] in
  assert_equal exp_clauses (BatDynArray.to_list p.TP.prob.Prob.clauses)

let test_nested_include_with_sel () =
  let prob =
    Tptp_prob.of_file clausify base_dir
      (base_dir ^ "04_test_nested_include_with_sel.p") in

  let p = Ast.Plain_word (Ast.to_plain_word "p") in
  let a = Ast.Plain_word (Ast.to_plain_word "a") in
  let b = Ast.Plain_word (Ast.to_plain_word "b") in
  let c = Ast.Plain_word (Ast.to_plain_word "c") in
  let d = Ast.Plain_word (Ast.to_plain_word "d") in

  (* TPTP symbols. *)
  let p1 = TP.Atomic_word (p, 1) in
  let a0 = TP.Atomic_word (a, 0) in
  let b0 = TP.Atomic_word (b, 0) in
  let c0 = TP.Atomic_word (c, 0) in
  let d0 = TP.Atomic_word (d, 0) in

  (* All TPTP symbols are mapped. *)
  assert_equal 5 (Hashtbl.length prob.TP.smap.TP.of_tptp);
  assert_equal 5 (Hashtbl.length prob.TP.smap.TP.to_tptp);

  (* Find ids of TPTP symbols. *)
  let find = Hashtbl.find prob.TP.smap.TP.of_tptp in
  let p1' = find p1 in
  let a0' = find a0 in
  let b0' = find b0 in
  let c0' = find c0 in
  let d0' = find d0 in

  (* Ids are different. *)
  assert_equal 5 (List.length (BatList.unique [p1'; a0'; b0'; c0'; d0']));

  (* Hashtable to_tptp is inverse of of_tptp
     (BTW this implies that ids are different).
  *)
  Hashtbl.iter
    (fun k v -> assert_equal k (Hashtbl.find prob.TP.smap.TP.to_tptp v))
    prob.TP.smap.TP.of_tptp;

  (* Array distinct_consts. *)
  assert_equal
    []
    (distinct_consts prob);

  assert_equal false prob.TP.has_conjecture;

  (* Clauses. *)
  let exp_clauses =
    let p a = L.lit (Sh.Pos, p1', [| a |]) in
    let const s = T.func (s, [| |]) in
    [
      {
        C.cl_id = 0;
        C.cl_lits = [ p (const a0') ];
      };
      {
        C.cl_id = 1;
        C.cl_lits = [ p (const b0') ];
      };
      {
        C.cl_id = 2;
        C.cl_lits = [ p (const c0') ];
      };
      {
        C.cl_id = 3;
        C.cl_lits = [ p (const d0') ];
      };
  ] in
  assert_equal exp_clauses (BatDynArray.to_list prob.TP.prob.Prob.clauses)

let test_fof_with_conjecture () =
  let nclausified = ref ~-1 in
  let clausify formulas =
    nclausified := List.length formulas;
    [] in
  let prob =
    Tptp_prob.of_file clausify base_dir
      (base_dir ^ "05_test_fof_with_conjecture.p") in

  let q = Ast.Plain_word (Ast.to_plain_word "q") in
  let c = Ast.Plain_word (Ast.to_plain_word "c") in

  (* TPTP symbols. *)
  let q2 = TP.Atomic_word (q, 2) in
  let c0 = TP.Atomic_word (c, 0) in

  (* All TPTP symbols are mapped. *)
  assert_equal 2 (Hashtbl.length prob.TP.smap.TP.of_tptp);
  assert_equal 2 (Hashtbl.length prob.TP.smap.TP.to_tptp);

  (* Find ids of TPTP symbols. *)
  let find = Hashtbl.find prob.TP.smap.TP.of_tptp in
  let q2' = find q2 in
  let c0' = find c0 in

  (* Ids are different. *)
  assert_equal 2 (List.length (BatList.unique [q2'; c0']));

  (* Hashtable to_tptp is inverse of of_tptp
     (BTW this implies that ids are different).
  *)
  Hashtbl.iter
    (fun k v -> assert_equal k (Hashtbl.find prob.TP.smap.TP.to_tptp v))
    prob.TP.smap.TP.of_tptp;

  (* Array distinct_consts. *)
  assert_equal
    []
    (distinct_consts prob);

  assert_equal true prob.TP.has_conjecture;
  assert_equal 2 !nclausified;

  (* No clauses - current clausification throws everything away. *)
  assert_equal [] (BatDynArray.to_list prob.TP.prob.Prob.clauses)

let test_fof_without_conjecture () =
  let nclausified = ref ~-1 in
  let clausify formulas =
    nclausified := List.length formulas;
    [] in
  let prob =
    Tptp_prob.of_file clausify base_dir
      (base_dir ^ "06_test_fof_without_conjecture.p") in

  let s = Ast.Plain_word (Ast.to_plain_word "s") in

  (* TPTP symbols. *)
  let s1 = TP.Atomic_word (s, 1) in
  let n0 = TP.Number Q.zero in
  let n1 = TP.Number Q.one in
  let n2 = TP.Number (Q.of_int 2) in

  (* All TPTP symbols are mapped. *)
  assert_equal 4 (Hashtbl.length prob.TP.smap.TP.of_tptp);
  assert_equal 4 (Hashtbl.length prob.TP.smap.TP.to_tptp);

  (* Find ids of TPTP symbols. *)
  let find = Hashtbl.find prob.TP.smap.TP.of_tptp in
  let s1' = find s1 in
  let n0' = find n0 in
  let n1' = find n1 in
  let n2' = find n2 in

  (* Ids are different. *)
  assert_equal 4 (List.length (BatList.unique [s1'; n0'; n1'; n2';]));

  (* Hashtable to_tptp is inverse of of_tptp
     (BTW this implies that ids are different).
  *)
  Hashtbl.iter
    (fun k v -> assert_equal k (Hashtbl.find prob.TP.smap.TP.to_tptp v))
    prob.TP.smap.TP.of_tptp;

  (* Array distinct_consts. *)
  assert_equal
    [n0'; n1'; n2']
    (distinct_consts prob);

  assert_equal false prob.TP.has_conjecture;
  (* 4 inputs are given to the clausifier - 3 formulas and 1 comment. *)
  assert_equal 4 !nclausified;

  (* No clauses - current clausification throws everything away. *)
  assert_equal [] (BatDynArray.to_list prob.TP.prob.Prob.clauses)

(* TODO: Tests for invalid inputs:
   - predicate symbol is already a function symbol and the other way around
   - syntax error
   - unsupported role
*)

let hashtbl_of_list xs = BatHashtbl.of_enum (BatList.enum xs)

let test_prob_to_tptp_vars () =
  let prob = Prob.create () in
  let db = prob.Prob.symbols in
  let fs = Symb.add_func db 2 in
  let f a b = T.func (fs, [| a; b |]) in
  let fsymb = Ast.Plain_word (Ast.to_plain_word "f") in
  let ftptp = TP.Atomic_word (fsymb, 2) in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    C.cl_lits = [
      L.mk_eq (f (T.var ~-2) (f (T.var 0) (T.var 5))) (T.var 29);
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;
  let tptp_prob = {
    TP.prob;
    TP.smap = {
      TP.of_tptp = hashtbl_of_list [ftptp, fs];
      TP.to_tptp = hashtbl_of_list [fs, ftptp];
    };
    TP.has_conjecture = false;
  } in
  let exp = [
    Ast.Cnf_anno {
      Ast.af_name = Ast.N_word (Ast.to_plain_word "cl");
      Ast.af_role = Ast.R_axiom;
      Ast.af_formula = Ast.Clause [
        let v s = Ast.Var (Ast.to_var s) in
        let l =
          Ast.Func (fsymb, [
              v "Y2"; Ast.Func (fsymb, [v "A"; v "F"])
            ]) in
        let r = v "X29" in
        Ast.Lit (Ast.Pos, Ast.Equals (l, r));
      ];
      Ast.af_annos = None;
    };
  ] in
  let res = ref [] in
  let seen_symbs =
    TP.prob_to_tptp
      tptp_prob
      Tptp_prob.Export_flat
      (fun cl -> res := cl :: !res) in
  assert_equal exp (List.rev !res);
  assert_seen_symbs [fs] seen_symbs

let test_prob_to_tptp_aux_symbs () =
  let prob = Prob.create () in
  let db = prob.Prob.symbols in
  (* Auxiliary. *)
  let fs = Symb.add_func db 2 in
  let f a b = T.func (fs, [| a; b |]) in
  let fsymb = Ast.Plain_word (Ast.to_plain_word "z1") in
  (* Auxiliary. *)
  let gs = Symb.add_func db 1 in
  Symb.set_auxiliary db gs true;
  let g a = T.func (gs, [| a |]) in
  let gsymb = Ast.Plain_word (Ast.to_plain_word "z2") in
  (* Auxiliary. *)
  let hs = Symb.add_func db 1 in
  let h a = T.func (hs, [| a |]) in
  let hsymb = Ast.Plain_word (Ast.to_plain_word "z4") in
  (* Not auxiliary. Different arity. *)
  let z2s = Symb.add_pred db 3 in
  let z2symb = Ast.Plain_word (Ast.to_plain_word "z2") in
  let z2tptp = TP.Atomic_word (z2symb, 3) in
  (* Not auxiliary. Same arity. *)
  let z3s = Symb.add_func db 1 in
  let z3symb = Ast.Plain_word (Ast.to_plain_word "z3") in
  let z3tptp = TP.Atomic_word (z3symb, 1) in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    C.cl_lits = [
      L.mk_ineq (f (T.var 25) (g (T.var 26))) (h (T.var 24));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;
  let tptp_prob = {
    TP.prob;
    TP.smap = {
      TP.of_tptp = hashtbl_of_list [z2tptp, z2s; z3tptp, z3s];
      TP.to_tptp = hashtbl_of_list [z2s, z2tptp; z3s, z3tptp];
    };
    TP.has_conjecture = false;
  } in
  let exp = [
    Ast.Cnf_anno {
      Ast.af_name = Ast.N_word (Ast.to_plain_word "cl");
      Ast.af_role = Ast.R_axiom;
      Ast.af_formula = Ast.Clause [
        let v s = Ast.Var (Ast.to_var s) in
        let l =
          Ast.Func (fsymb, [
              v "Z"; Ast.Func (gsymb, [v "X26"])
            ]) in
        let r =  Ast.Func (hsymb, [v "Y"]) in
        Ast.Lit (Ast.Neg, Ast.Equals (l, r));
      ];
      Ast.af_annos = None;
    };
  ] in
  let res = ref [] in
  let seen_symbs =
    TP.prob_to_tptp
      tptp_prob
      Tptp_prob.Export_flat
      (fun cl -> res := cl :: !res) in
  assert_equal exp (List.rev !res);
  assert_seen_symbs [fs; gs; hs] seen_symbs

let test_prob_to_tptp_commutativity () =
  let prob = Prob.create () in
  let db = prob.Prob.symbols in
  let fs = Symb.add_func db 2 in
  Symb.set_commutative db fs true;
  let f a b = T.func (fs, [| a; b |]) in
  let fsymb = Ast.Plain_word (Ast.to_plain_word "f") in
  let ftptp = TP.Atomic_word (fsymb, 2) in
  let ps = Symb.add_pred db 0 in
  let p = L.lit (Sh.Pos, ps, [| |]) in
  let psymb = Ast.Plain_word (Ast.to_plain_word "p") in
  let ptptp = TP.Atomic_word (psymb, 0) in
  let cs = Symb.add_func db 0 in
  let c = T.func (cs, [| |]) in
  let csymb = Ast.String (Ast.to_tptp_string "const") in
  let ctptp = TP.String (Ast.to_tptp_string "const") in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    C.cl_lits = [
      p;
      L.mk_eq (f (T.var 1) c) c;
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;
  let tptp_prob = {
    TP.prob;
    TP.smap = {
      TP.of_tptp = hashtbl_of_list [ftptp, fs; ptptp, ps; ctptp, cs];
      TP.to_tptp = hashtbl_of_list [fs, ftptp; ps, ptptp; cs, ctptp];
    };
    TP.has_conjecture = false;
  } in
  let v s = Ast.Var (Ast.to_var s) in
  let exp_clause = Ast.Cnf_anno {
    Ast.af_name = Ast.N_word (Ast.to_plain_word "cl");
    Ast.af_role = Ast.R_axiom;
    Ast.af_formula = Ast.Clause [
      Ast.Lit (Ast.Pos, Ast.Pred (psymb, []));
      let l = Ast.Func (fsymb, [v "B"; csymb]) in
      let r = csymb in
      Ast.Lit (Ast.Pos, Ast.Equals (l, r));
    ];
    Ast.af_annos = None;
  } in
  let l = Ast.Func (fsymb, [v "A"; v "B"]) in
  let r = Ast.Func (fsymb, [v "B"; v "A"]) in
  let exp_comm = Ast.Cnf_anno {
    Ast.af_name = Ast.N_word (Ast.to_plain_word "cl");
    Ast.af_role = Ast.R_axiom;
    Ast.af_formula = Ast.Clause [
      Ast.Lit (Ast.Pos, Ast.Equals (l, r));
    ];
    Ast.af_annos = None;
  } in
  let exp_flat_comm = Ast.Cnf_anno {
    Ast.af_name = Ast.N_word (Ast.to_plain_word "cl");
    Ast.af_role = Ast.R_axiom;
    Ast.af_formula = Ast.Clause [
      Ast.Lit (Ast.Pos, Ast.Equals (v "C", l));
      Ast.Lit (Ast.Neg, Ast.Equals (v "C", r));
    ];
    Ast.af_annos = None;
  } in
  let res = ref [] in
  let seen_symbs =
    TP.prob_to_tptp
      tptp_prob
      Tptp_prob.Export
      (fun cl -> res := cl :: !res) in
  assert_equal [exp_clause; exp_comm] (List.rev !res);
  assert_seen_symbs [fs; ps; cs] seen_symbs;
  let res_flat = ref [] in
  let seen_symbs =
    TP.prob_to_tptp
      tptp_prob
      Tptp_prob.Export_flat
      (fun cl -> res_flat := cl :: !res_flat) in
  assert_equal [exp_clause; exp_flat_comm] (List.rev !res_flat);
  assert_seen_symbs [fs; ps; cs] seen_symbs

module M = Model

let map_of_list xs = Symb.Map.of_enum (BatList.enum xs)

let test_model_to_tptp () =
  let prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 1 in
  let q = Symb.add_pred db 0 in
  let c = Symb.add_func db 0 in
  Symb.set_distinct_constant db c true;
  let d = Symb.add_func db 0 in
  Symb.set_distinct_constant db d true;
  let f = Symb.add_func db 2 in

  let model = {
    M.max_size = 4;
    M.symbs =
      map_of_list [
        p, { M.values = [| 0; 1; 0; 0 |] };
        q, { M.values = [| 1 |] };
        c, { M.values = [| 2 |] };
        d, { M.values = [| 0 |] };
        f,
        { M.values = [| 2; 3; 2; 0; 1; 0; 0; 3; 1; 2; 0; 0; 3; 3; 2; 1 |] };
      ];
  } in

  let prob =
    let p' = TP.Atomic_word (Ast.Plain_word (Ast.to_plain_word "p"), 1) in
    let q' = TP.Atomic_word (Ast.Plain_word (Ast.to_plain_word "q"), 0) in
    let c' = TP.Number (Q.of_int 0) in
    let d' = TP.String (Ast.to_tptp_string "bar") in
    let f' = TP.Atomic_word (Ast.Plain_word (Ast.to_plain_word "f"), 2) in
    let smap =
      let pairs = [p', p; q', q; c', c; d', d; f', f] in
      {
        TP.of_tptp = hashtbl_of_list pairs;
        TP.to_tptp =
          hashtbl_of_list (BatList.map (fun (a, b) -> (b, a)) pairs);
      } in
    {
      TP.smap;
      TP.prob;
      TP.has_conjecture = false;
    } in

  let interp_name = Ast.N_word (Ast.to_plain_word "interp") in

  let exp_formulas =
    let p' = Ast.Plain_word (Ast.to_plain_word "p") in
    let q' = Ast.Plain_word (Ast.to_plain_word "q") in
    let f' = Ast.Plain_word (Ast.to_plain_word "f") in
    let d0, d1, d2, d3 =
      Ast.String (Ast.to_tptp_string "bar"),
      Ast.Number (Q.of_int 1),
      Ast.Number (Q.of_int 0),
      Ast.Number (Q.of_int 2) in
    let eq t t' = Ast.Atom (Ast.Equals (t, t')) in
    let f args res = eq (Ast.Func (f', args)) res in
    let dom =
      let x = Ast.to_var "X" in
      let disjunction =
        let x = Ast.Var x in
        BatList.reduce
          (fun a b -> Ast.Binop (Ast.Or, a, b))
          [eq x d0; eq x d1; eq x d2; eq x d3] in
      Ast.Fof_anno
        {
          Ast.af_name = interp_name;
          Ast.af_role = Ast.R_fi_domain;
          Ast.af_formula = Ast.Formula (Ast.Quant (Ast.All, x, disjunction));
          Ast.af_annos = None;
        } in
    let wrap_func lit =
      Ast.Fof_anno
        {
          Ast.af_name = interp_name;
          Ast.af_role = Ast.R_fi_functors;
          Ast.af_formula = Ast.Formula lit;
          Ast.af_annos = None;
        } in
    let func_f =
      [
        f [d0; d0] d2; f [d0; d1] d3; f [d0; d2] d2; f [d0; d3] d0;
        f [d1; d0] d1; f [d1; d1] d0; f [d1; d2] d0; f [d1; d3] d3;
        f [d2; d0] d1; f [d2; d1] d2; f [d2; d2] d0; f [d2; d3] d0;
        f [d3; d0] d3; f [d3; d1] d3; f [d3; d2] d2; f [d3; d3] d1;
      ]
      |> BatList.map wrap_func in
    let wrap_pred lit =
      Ast.Fof_anno
        {
          Ast.af_name = interp_name;
          Ast.af_role = Ast.R_fi_predicates;
          Ast.af_formula = Ast.Formula lit;
          Ast.af_annos = None;
        } in
    let pred_p =
      [
        Ast.Not (Ast.Atom (Ast.Pred (p', [d0])));
        Ast.Atom (Ast.Pred (p', [d1]));
        Ast.Not (Ast.Atom (Ast.Pred (p', [d2])));
        Ast.Not (Ast.Atom (Ast.Pred (p', [d3])));
      ]
      |> BatList.map wrap_pred in
    let pred_q = Ast.Atom (Ast.Pred (q', [])) |> wrap_pred in
    [
      Ast.Comment (Ast.to_comment_line " SZS output start FiniteModel");
      Ast.Comment (Ast.to_comment_line " domain size: 4");
      dom;
    ] @ func_f @ pred_p @
    [
      pred_q;
      Ast.Comment (Ast.to_comment_line " SZS output end FiniteModel");
    ] in

  let formulas =
    let fs = ref [] in
    TP.model_to_tptp prob model interp_name (fun f -> fs := f :: !fs);
    List.rev !fs in

  assert_equal exp_formulas formulas

let hashtbl_to_list xs =
  xs
  |> BatHashtbl.enum
  |> BatList.of_enum
  |> BatList.sort compare

let test_restrict_symb_map () =
  let db = Symb.create_db () in
  let p = Symb.add_pred db 1 in
  let c = Symb.add_func db 0 in
  Symb.set_distinct_constant db c true;
  let f = Symb.add_func db 2 in
  let p' = TP.Atomic_word (Ast.Plain_word (Ast.to_plain_word "p"), 1) in
  let c' = TP.String (Ast.to_tptp_string "c") in
  let f' = TP.Atomic_word (Ast.Plain_word (Ast.to_plain_word "f"), 2) in
  let smap =
    let pairs = [p', p; c', c; f', f] in
    {
      TP.of_tptp = hashtbl_of_list pairs;
      TP.to_tptp = hashtbl_of_list (BatList.map (fun (a, b) -> b, a) pairs);
    } in
  let rsmap =
    let symbs = [p; f] |> BatList.enum |> Symb.Set.of_enum in
    TP.restrict_symb_map symbs smap in
  let rsmap2 = TP.restrict_symb_map Symb.Set.empty smap in
  assert_equal [f', f; p', p] (hashtbl_to_list rsmap.TP.of_tptp);
  assert_equal [p, p'; f, f'] (hashtbl_to_list rsmap.TP.to_tptp);
  assert_equal [] (hashtbl_to_list rsmap2.TP.of_tptp);
  assert_equal [] (hashtbl_to_list rsmap2.TP.to_tptp)

let suite =
  "Tptp_prob suite" >:::
    [
      "of_file - basic" >:: test_basic;
      "of_file - include" >:: test_include;
      "of_file - nested include" >:: test_nested_include;
      "of_file - nested include with selection" >::
        test_nested_include_with_sel;
      "of_file - FOF with conjecture" >:: test_fof_with_conjecture;
      "of_file - FOF without conjecture" >:: test_fof_without_conjecture;
      "prob_to_tptp - vars" >:: test_prob_to_tptp_vars;
      "prob_to_tptp - aux symbs" >:: test_prob_to_tptp_aux_symbs;
      "prob_to_tptp - commutativity" >:: test_prob_to_tptp_commutativity;
      "model_to_tptp" >:: test_model_to_tptp;
      "restrict_symb_map" >:: test_restrict_symb_map;
    ]
