(* Copyright (c) 2013 Radek Micek *)

open OUnit

module Ast = Tptp_ast
module TP = Tptp_prob
module T = Term
module L = Lit
module C = Clause2

let base_dir = "test/data/tptp_prob/"

let test_basic () =
  let Tptp_prob.Wr p =
    Tptp_prob.of_file base_dir (base_dir ^ "01_test_basic.p") in

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
     (btw this implies that ids array different).
  *)
  Hashtbl.iter
    (fun k v -> assert_equal k (Hashtbl.find p.TP.smap.TP.to_tptp v))
    p.TP.smap.TP.of_tptp;

  (* Hashtable preds. *)
  assert_equal 6 (Hashtbl.length p.TP.preds);
  assert_bool "" (Hashtbl.find p.TP.preds q2);
  assert_bool "" (Hashtbl.find p.TP.preds q1);
  assert_bool "" (not (Hashtbl.find p.TP.preds c0));
  assert_bool "" (not (Hashtbl.find p.TP.preds d0));
  assert_bool "" (not (Hashtbl.find p.TP.preds f2));
  assert_bool "" (not (Hashtbl.find p.TP.preds f1));

  (* Array distinct_consts. *)
  assert_equal
    [str_hi'; num_twelve_point_five'; num_seven'; str_hello_world']
    (BatDynArray.to_list p.TP.prob.Prob.distinct_consts);

  (* Clauses. *)
  let q2 a b = L.Lit (Sh.Pos, q2', [| a; b |]) in
  let q1 a = L.Lit (Sh.Pos, q1', [| a |]) in
  let c0 = T.Func (c0', [| |]) in
  let d0 = T.Func (d0', [| |]) in
  let f2 a b = T.Func (f2', [| a; b |]) in
  let f1 a = T.Func (f1', [| a |]) in
  let exp_clauses = [
    {
      C.cl_id = 0;
      C.cl_lits = [ q2 (T.Var 0) c0 ];
    };
    {
      C.cl_id = 1;
      C.cl_lits = [ L.neg (q2 c0 d0) ];
    };
    {
      C.cl_id = 2;
      C.cl_lits = [
        L.mk_eq (T.Var 0) (T.Var 1);
        L.mk_ineq
          (f2 (T.Var 0) (T.Var 1))
          (f2 (T.Var 1) (T.Var 0));
      ];
    };
    {
      C.cl_id = 3;
      C.cl_lits = [
        L.neg
          (q2
             (T.Func (str_hi', [| |]))
             (T.Func (num_twelve_point_five', [| |])));
      ];
    };
    {
      C.cl_id = 4;
      C.cl_lits = [
        q1 (T.Func (num_seven', [| |]));
        L.mk_eq
          (f1 (T.Var 0))
          (T.Func (str_hello_world', [| |]));
      ];
    };
  ] in
  assert_equal exp_clauses (BatDynArray.to_list p.TP.prob.Prob.clauses)

let test_include () =
  let Tptp_prob.Wr p =
    Tptp_prob.of_file base_dir (base_dir ^ "02_test_include.p") in

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
     (btw this implies that ids array different).
  *)
  Hashtbl.iter
    (fun k v -> assert_equal k (Hashtbl.find p.TP.smap.TP.to_tptp v))
    p.TP.smap.TP.of_tptp;

  (* Hashtable preds. *)
  assert_equal 4 (Hashtbl.length p.TP.preds);
  assert_bool "" (Hashtbl.find p.TP.preds q1);
  assert_bool "" (not (Hashtbl.find p.TP.preds c0));
  assert_bool "" (not (Hashtbl.find p.TP.preds d0));
  assert_bool "" (not (Hashtbl.find p.TP.preds g1));

  (* Array distinct_consts. *)
  assert_equal
    [num_zero'; num_one']
    (BatDynArray.to_list p.TP.prob.Prob.distinct_consts);

  (* Clauses. *)
  let q1 a = L.Lit (Sh.Pos, q1', [| a |]) in
  let c0 = T.Func (c0', [| |]) in
  let d0 = T.Func (d0', [| |]) in
  let g1 a = T.Func (g1', [| a |]) in
  let num_zero = T.Func (num_zero', [| |]) in
  let num_one = T.Func (num_one', [| |]) in
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
  let Tptp_prob.Wr p =
    Tptp_prob.of_file base_dir (base_dir ^ "03_test_nested_include.p") in

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
     (btw this implies that ids array different).
  *)
  Hashtbl.iter
    (fun k v -> assert_equal k (Hashtbl.find p.TP.smap.TP.to_tptp v))
    p.TP.smap.TP.of_tptp;

  (* Hashtable preds. *)
  assert_equal 2 (Hashtbl.length p.TP.preds);
  assert_bool "" (Hashtbl.find p.TP.preds r0);
  assert_bool "" (Hashtbl.find p.TP.preds s0);

  (* Array distinct_consts. *)
  assert_equal
    []
    (BatDynArray.to_list p.TP.prob.Prob.distinct_consts);

  (* Clauses. *)
  let exp_clauses = [
    {
      C.cl_id = 0;
      C.cl_lits = [
        L.Lit (Sh.Pos, r0', [| |]);
        L.neg (L.Lit (Sh.Pos, s0', [| |]))
      ];
    };
  ] in
  assert_equal exp_clauses (BatDynArray.to_list p.TP.prob.Prob.clauses)

let test_nested_include_with_sel () =
  let Tptp_prob.Wr prob =
    Tptp_prob.of_file base_dir
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
     (btw this implies that ids array different).
  *)
  Hashtbl.iter
    (fun k v -> assert_equal k (Hashtbl.find prob.TP.smap.TP.to_tptp v))
    prob.TP.smap.TP.of_tptp;

  (* Array distinct_consts. *)
  assert_equal
    []
    (BatDynArray.to_list prob.TP.prob.Prob.distinct_consts);

  (* Clauses. *)
  let exp_clauses =
    let p a = L.Lit (Sh.Pos, p1', [| a |]) in
    let const s = T.Func (s, [| |]) in
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

(* TODO: Tests for invalid inputs:
   - predicate symbol is already a function symbol and the other way around
   - syntax error
   - fof in the input
   - unsupported role
*)

module M = Model

let hashtbl_of_list xs = BatHashtbl.of_enum (BatList.enum xs)

let test_model_to_tptp () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 1 in
  let q = Symb.add_pred db 0 in
  let c = Symb.add_func db 0 in
  let d = Symb.add_func db 0 in
  let f = Symb.add_func db 2 in

  let model = {
    M.max_size = 4;
    M.symbs =
      hashtbl_of_list [
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
    let preds =
      hashtbl_of_list [p', true; q', true; c', false; d', false; f', false] in
    {
      TP.smap;
      TP.preds;
      TP.prob;
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
      Ast.Formula (Ast.Quant (Ast.All, x, disjunction)) in
    let func_f =
      let conjunction =
        BatList.reduce
          (fun a b -> Ast.Binop (Ast.And, a, b))
          [
            f [d0; d0] d2; f [d0; d1] d3; f [d0; d2] d2; f [d0; d3] d0;
            f [d1; d0] d1; f [d1; d1] d0; f [d1; d2] d0; f [d1; d3] d3;
            f [d2; d0] d1; f [d2; d1] d2; f [d2; d2] d0; f [d2; d3] d0;
            f [d3; d0] d3; f [d3; d1] d3; f [d3; d2] d2; f [d3; d3] d1;
          ] in
      Ast.Formula conjunction in
    let pred_p =
      let conjunction =
        BatList.reduce
          (fun a b -> Ast.Binop (Ast.And, a, b))
          [
            Ast.Not (Ast.Atom (Ast.Pred (p', [d0])));
            Ast.Atom (Ast.Pred (p', [d1]));
            Ast.Not (Ast.Atom (Ast.Pred (p', [d2])));
            Ast.Not (Ast.Atom (Ast.Pred (p', [d3])));
          ] in
      Ast.Formula conjunction in
    let pred_q = Ast.Formula (Ast.Atom (Ast.Pred (q', []))) in
    [
      Ast.Fof_anno
        {
          Ast.af_name = interp_name;
          Ast.af_role = Ast.R_fi_domain;
          Ast.af_formula = dom;
          Ast.af_annos = None;
        };
      Ast.Fof_anno
        {
          Ast.af_name = interp_name;
          Ast.af_role = Ast.R_fi_functors;
          Ast.af_formula = func_f;
          Ast.af_annos = None;
        };
      Ast.Fof_anno
        {
          Ast.af_name = interp_name;
          Ast.af_role = Ast.R_fi_predicates;
          Ast.af_formula = pred_p;
          Ast.af_annos = None;
        };
      Ast.Fof_anno
        {
          Ast.af_name = interp_name;
          Ast.af_role = Ast.R_fi_predicates;
          Ast.af_formula = pred_q;
          Ast.af_annos = None;
        };
      Ast.Comment (Ast.to_comment_line " domain size: 4");
    ] in

  let formulas =
    let fs = ref [] in
    TP.model_to_tptp prob model interp_name (fun f -> fs := f :: !fs);
    BatList.sort !fs in

  assert_equal exp_formulas formulas

let suite =
  "Tptp_prob suite" >:::
    [
      "of_file - basic" >:: test_basic;
      "of_file - include" >:: test_include;
      "of_file - nested include" >:: test_nested_include;
      "of_file - nested include with selection" >::
        test_nested_include_with_sel;
      "model_to_tptp" >:: test_model_to_tptp;
    ]
