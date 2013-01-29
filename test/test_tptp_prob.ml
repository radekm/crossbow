(* Copyright (c) 2013 Radek Micek *)

open OUnit

module Ast = Tptp_ast
module TP = Tptp_prob
module T = Term
module C = Clause

let base_dir = "test/data/tptp_prob"

let test_basic () =
  let p = Tptp_prob.of_file base_dir "01_test_basic.p" in

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
  let q2 a b = T.Func (q2', [| a; b |]) in
  let q1 a = T.Func (q1', [| a |]) in
  let c0 = T.Func (c0', [| |]) in
  let d0 = T.Func (d0', [| |]) in
  let f2 a b = T.Func (f2', [| a; b |]) in
  let f1 a = T.Func (f1', [| a |]) in
  let exp_clauses = [
    {
      Clause.cl_id = 0;
      Clause.cl_lits = [ q2 (T.Var 0) c0 ];
    };
    {
      Clause.cl_id = 1;
      Clause.cl_lits = [ C.neg_lit (q2 c0 d0) ];
    };
    {
      Clause.cl_id = 2;
      Clause.cl_lits = [
        T.mk_eq (T.Var 0) (T.Var 1);
        T.mk_ineq
          (f2 (T.Var 0) (T.Var 1))
          (f2 (T.Var 1) (T.Var 0));
      ];
    };
    {
      Clause.cl_id = 3;
      Clause.cl_lits = [
        C.neg_lit
          (q2
             (T.Func (str_hi', [| |]))
             (T.Func (num_twelve_point_five', [| |])));
      ];
    };
    {
      Clause.cl_id = 4;
      Clause.cl_lits = [
        q1 (T.Func (num_seven', [| |]));
        T.mk_eq
          (f1 (T.Var 0))
          (T.Func (str_hello_world', [| |]));
      ];
    };
  ] in
  assert_equal exp_clauses (BatDynArray.to_list p.TP.prob.Prob.clauses)

let test_include () =
  let p = Tptp_prob.of_file base_dir "02_test_include.p" in

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
  let q1 a = T.Func (q1', [| a |]) in
  let c0 = T.Func (c0', [| |]) in
  let d0 = T.Func (d0', [| |]) in
  let g1 a = T.Func (g1', [| a |]) in
  let num_zero = T.Func (num_zero', [| |]) in
  let num_one = T.Func (num_one', [| |]) in
  let exp_clauses = [
    {
      Clause.cl_id = 0;
      Clause.cl_lits = [ T.mk_eq (g1 c0) num_zero ];
    };
    {
      Clause.cl_id = 1;
      Clause.cl_lits = [ T.mk_eq c0 (g1 (g1 c0)) ];
    };
    {
      Clause.cl_id = 2;
      Clause.cl_lits = [ q1 c0 ];
    };
    {
      Clause.cl_id = 3;
      Clause.cl_lits = [ T.mk_ineq d0 c0 ];
    };
    {
      Clause.cl_id = 4;
      Clause.cl_lits = [ T.mk_eq d0 num_one ];
    };
    {
      Clause.cl_id = 5;
      Clause.cl_lits = [ q1 d0 ];
    };
  ] in
  assert_equal exp_clauses (BatDynArray.to_list p.TP.prob.Prob.clauses)

let test_nested_include () =
  let p = Tptp_prob.of_file base_dir "03_test_nested_include.p" in

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
      Clause.cl_id = 0;
      Clause.cl_lits = [ T.Func (r0', [| |]); C.neg_lit (T.Func (s0', [| |])) ];
    };
  ] in
  assert_equal exp_clauses (BatDynArray.to_list p.TP.prob.Prob.clauses)

(* TODO: Tests for invalid inputs:
   - predicate symbol is already a function symbol and the other way around
   - syntax error
   - fof in the input
   - unsupported role
*)

let suite =
  "Tptp_prob suite" >:::
    [
      "of_file - basic" >:: test_basic;
      "of_file - include" >:: test_include;
      "of_file - nested include" >:: test_nested_include;
    ]
