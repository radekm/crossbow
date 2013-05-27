(* Copyright (c) 2013 Radek Micek *)

open OUnit

module T = Term
module C = Clause

let (|>) = BatPervasives.(|>)

let get_auxiliary_symbs db =
  let aux = ref [] in
  Symb.iter
    (fun s ->
      if Symb.auxiliary db s then
        aux := s :: !aux)
    db;
  BatList.sort !aux

let show_clauses cs =
  cs
  |> BatList.map (fun cl -> C.show cl ^ "\n")
  |> String.concat ""
  |> (fun str -> "\n" ^ str)

let test_paradox_splitting_empty_cl () =
  let prob = Prob.create () in
  let cl = {
    C.cl_id = Prob.fresh_id prob;
    C.cl_lits = [];
  } in
  let clauses = Splitting.split_clause Splitting.paradox_splitting prob cl in
  let exp_clauses = [
    {
      C.cl_id = 1;
      C.cl_lits = [];
    };
  ] in
  assert_equal ~printer:show_clauses exp_clauses clauses

let make_cl () =
  let prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_anon_symb db 2 in
  let p a b = T.Func (p, [| a; b |]) in
  let f = Symb.add_anon_symb db 5 in
  let f a b c d e = T.Func (f, [| a; b; c; d; e |]) in
  let x i = T.Var i in
  let cl = {
    C.cl_id = Prob.fresh_id prob;
    C.cl_lits = [
      C.neg_lit (p (x 0) (x 1));
      C.neg_lit (p (x 2) (x 3));
      C.neg_lit (p (x 4) (x 5));
      C.neg_lit (p (x 6) (x 7));
      C.neg_lit (p (x 8) (x 9));
      p (x 10) (x 11);
      T.mk_ineq (x 10) (f (x 0) (x 2) (x 4) (x 6) (x 8));
      T.mk_ineq (x 11) (f (x 1) (x 3) (x 5) (x 7) (x 9));
    ];
  } in
  prob, db, cl, p, f, x

let test_paradox_splitting () =
  let prob, db, cl, p, f, x = make_cl () in
  let clauses = Splitting.split_clause Splitting.paradox_splitting prob cl in
  let q1, q2, q3, q4, q5 =
    match get_auxiliary_symbs db with
      | [q1; q2; q3; q4; q5] -> q1, q2, q3, q4, q5
      | _ -> assert_failure "auxiliary symbols" in
  let normalize cl = fst (C.normalize_vars cl) in
  let exp_clauses = [
    normalize {
      C.cl_id = 1;
      C.cl_lits = [
        T.Func (q1, [| x 1; x 2; x 4; x 6; x 8; x 10 |]);
        C.neg_lit (p (x 0) (x 1));
        T.mk_ineq (x 10) (f (x 0) (x 2) (x 4) (x 6) (x 8));
      ];
    };
    normalize {
      C.cl_id = 2;
      C.cl_lits = [
        T.Func (q2, [| x 0; x 2; x 3; x 4; x 5; x 6 |]);
        C.neg_lit (T.Func (q1, [| x 0; x 1; x 2; x 3; x 4; x 5 |]));
        C.neg_lit (p (x 1) (x 6));
      ];
    };
    normalize {
      C.cl_id = 3;
      C.cl_lits = [
        T.Func (q3, [| x 0; x 2; x 3; x 4; x 5; x 6 |]);
        C.neg_lit (T.Func (q2, [| x 0; x 1; x 2; x 3; x 4; x 5 |]));
        C.neg_lit (p (x 1) (x 6));
      ];
    };
    normalize {
      C.cl_id = 4;
      C.cl_lits = [
        T.Func (q4, [| x 0; x 2; x 3; x 4; x 5; x 6 |]);
        C.neg_lit (T.Func (q3, [| x 0; x 1; x 2; x 3; x 4; x 5 |]));
        C.neg_lit (p (x 1) (x 6));

      ];
    };
    normalize {
      C.cl_id = 5;
      C.cl_lits = [
        T.Func (q5, [| x 0; x 2; x 3; x 4; x 5; x 6 |]);
        C.neg_lit (T.Func (q4, [| x 0; x 1; x 2; x 3; x 4; x 5 |]));
        C.neg_lit (p (x 1) (x 6));
      ];
    };
    normalize {
      C.cl_id = 6;
      C.cl_lits = [
        C.neg_lit (T.Func (q5, [| x 0; x 2; x 3; x 4; x 5; x 6 |]));
        p (x 2) (x 7);
        T.mk_ineq (x 7) (f (x 0) (x 3) (x 4) (x 5) (x 6));
      ];
    };
  ] in
  assert_equal ~printer:show_clauses exp_clauses clauses

let make_cl2 () =
  let prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_anon_symb db 2 in
  let p a b = T.Func (p, [| a; b |]) in
  let q = Symb.add_anon_symb db 4 in
  let q a b c d = T.Func (q, [| a; b; c; d |]) in
  let r = Symb.add_anon_symb db 0 in
  let r = T.Func (r, [| |]) in
  let x i = T.Var i in
  let cl = {
    C.cl_id = Prob.fresh_id prob;
    C.cl_lits = [
      p (x 0) (x 1);
      p (x 0) (x 2);
      p (x 1) (x 2);
      r;
      q (x 3) (x 4) (x 5) (x 6);
    ];
  } in
  prob, db, cl, p, q, r, x

let test_paradox_splitting2 () =
  let prob, db, cl, p, q, r, x = make_cl2 () in
  let clauses = Splitting.split_clause Splitting.paradox_splitting prob cl in
  let q1, q2 =
    match get_auxiliary_symbs db with
      | [q1; q2] -> q1, q2
      | _ -> assert_failure "auxiliary symbols" in
  let normalize cl = fst (C.normalize_vars cl) in
  let exp_clauses = [
    normalize {
      C.cl_id = 1;
      C.cl_lits = [
        T.Func (q1, [| x 1; x 2 |]);
        p (x 0) (x 1);
        p (x 0) (x 2);
      ];
    };
    normalize {
      C.cl_id = 2;
      C.cl_lits = [
        T.Func (q2, [| |]);
        C.neg_lit (T.Func (q1, [| x 0; x 1 |]));
        p (x 0) (x 1);
      ];
    };
    normalize {
      C.cl_id = 3;
      C.cl_lits = [
        C.neg_lit (T.Func (q2, [| |]));
        r;
        q (x 2) (x 3) (x 4) (x 5);
      ];
    };
  ] in
  assert_equal ~printer:show_clauses exp_clauses clauses


let suite =
  "Splitting suite" >:::
    [
      "paradox_splitting - empty clause" >:: test_paradox_splitting_empty_cl;
      "paradox_splitting" >:: test_paradox_splitting;
      "paradox_splitting 2" >:: test_paradox_splitting2;
    ]
