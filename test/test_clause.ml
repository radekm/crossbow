(* Copyright (c) 2013 Radek Micek *)

open OUnit

module S = Symb
module T = Term
module C = Clause

let test_neg_lit1 () =
  assert_equal
    (T.mk_eq (T.Var 0) (T.Var 1))
    (C.neg_lit (T.mk_ineq (T.Var 0) (T.Var 1)))

let test_neg_lit2 () =
  let db = S.create_db () in
  let p = S.add_symb db "p" 1 in
  let term = T.Func (p, [| T.Var 2 |]) in
  assert_equal
    (T.Func (S.sym_not, [| term |]))
    (C.neg_lit term)

let test_true_lit_false_lit () =
  let db = S.create_db () in
  let f = S.add_symb db "f" 1 in
  let mk_term v = T.Func (f, [| T.Var v |]) in
  (* true_lit *)
  assert_bool "" (C.true_lit (T.mk_eq (mk_term 1) (mk_term 1)));
  assert_bool "" (not (C.true_lit (T.mk_eq (mk_term 2) (mk_term 1))));
  assert_bool "" (not (C.true_lit (T.mk_ineq (mk_term 1) (mk_term 1))));
  (* false_lit *)
  assert_bool "" (C.false_lit (T.mk_ineq (mk_term 1) (mk_term 1)));
  assert_bool "" (not (C.false_lit (T.mk_ineq (mk_term 2) (mk_term 1))));
  assert_bool "" (not (C.false_lit (T.mk_eq (mk_term 1) (mk_term 1))))

let test_simplify () =
  let db = S.create_db () in
  let f = S.add_symb db "f" 2 in
  let g = S.add_symb db "g" 2 in
  S.set_commutative db f true;
  let f a b = T.Func (f, [| a; b |]) in
  let g a b = T.Func (g, [| a; b |]) in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let z = T.Var 2 in
  (* f(x, y) != f(y, z), z != y, g(x, y) != g(y, x), g(y, x) != g(x, z), z = x *)
  let orig_clause = {
    C.cl_id = 1;
    C.cl_lits = [
      T.mk_ineq (f x y) (f x z);
      T.mk_ineq z y;
      T.mk_ineq (g x y) (g y x);
      T.mk_ineq (g y x) (g x z);
      T.mk_eq z x;
    ];
  } in
  (* g(x, y) != g(y, x), x = y *)
  let simpl_clause = {
    C.cl_id = 1;
    C.cl_lits = [
      T.mk_ineq (g x y) (g y x);
      T.mk_eq x y;
    ];
  } in
  assert_equal
    (Some simpl_clause)
    (C.simplify db orig_clause)

let test_simplify_tautology () =
  let db = S.create_db () in
  let f = S.add_symb db "f" 1 in
  let f a = T.Func (f, [| a |]) in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let z = T.Var 2 in
  (* f(y) = f(x), f(z) != f(y), x != z *)
  assert_equal
    None
    (C.simplify db {
      C.cl_id = 1;
      C.cl_lits = [
        T.mk_eq (f y) (f x);
        T.mk_ineq (f z) (f y);
        T.mk_ineq x z;
      ];
    });
  (* f(x) = f(z), x != y, y != z *)
  assert_equal
    None
    (C.simplify db {
      C.cl_id = 2;
      C.cl_lits = [
        T.mk_eq (f x) (f y);
        T.mk_ineq x y;
        T.mk_ineq y z;
      ];
    })

let test_normalize_vars () =
  let db = S.create_db () in
  let f = S.add_symb db "f" 2 in
  let f a b = T.Func (f, [| a; b |]) in
  let orig_clause = {
    C.cl_id = 2;
    C.cl_lits = [
      T.mk_eq (f (T.Var 3) (T.Var 3)) (T.Var 7);
      T.mk_ineq (T.Var 4) (T.Var 3);
      T.mk_ineq (f (T.Var 8) (T.Var 7)) (f (T.Var 0) (T.Var 3));
    ];
  } in
  let norm_clause = {
    C.cl_id = 2;
    C.cl_lits = [
      T.mk_eq (f (T.Var 0) (T.Var 0)) (T.Var 1);
      T.mk_ineq (T.Var 2) (T.Var 0);
      T.mk_ineq (f (T.Var 3) (T.Var 1)) (f (T.Var 4) (T.Var 0));
    ];
  } in
  assert_equal
    (norm_clause, 5)
    (C.normalize_vars orig_clause)

let suite =
  "Clause suite" >:::
    [
      "neg_lit 1" >:: test_neg_lit1;
      "neg_lit 2" >:: test_neg_lit2;
      "true_lit, false_lit" >:: test_true_lit_false_lit;
      "simplify" >:: test_simplify;
      "simplify tautology" >:: test_simplify_tautology;
      "normalize_vars" >:: test_normalize_vars;
    ]
