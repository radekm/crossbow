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

let test_flatten () =
  let db = S.create_db () in
  let f = S.add_symb db "f" 1 in
  let c = S.add_symb db "c" 0 in
  let d = S.add_symb db "d" 0 in
  let f a = T.Func (f, [| a |]) in
  let c = T.Func (c, [| |]) in
  let d = T.Func (d, [| |]) in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let v1 = T.Var 2 in
  let v2 = T.Var 3 in
  (* x != f(c), y != f(d), f(c) = f(d) *)
  let orig_clause = {
    C.cl_id = 0;
    C.cl_lits = [
      T.mk_ineq x (f c);
      T.mk_ineq y (f d);
      T.mk_eq (f c) (f d);
    ];
  } in
  (* v2 != d, v1 != c, x != f(v1), y != f(v2), x = y *)
  let flat_clause = {
    C.cl_id = 0;
    C.cl_lits = [
      T.mk_ineq v2 d;
      T.mk_ineq v1 c;
      T.mk_ineq x (f v1);
      T.mk_ineq y (f v2);
      T.mk_eq x y;
    ];
  } in
  assert_equal (Some flat_clause) (C.flatten db orig_clause)

let test_flatten_commutative_symb () =
  let db = S.create_db () in
  let f = S.add_symb db "f" 2 in
  S.set_commutative db f true;
  let p = S.add_symb db "p" 2 in
  let c = S.add_symb db "c" 0 in
  let d = S.add_symb db "d" 0 in
  let f a b = T.Func (f, [| a; b |]) in
  let p a b = T.Func (p, [| a; b |]) in
  let c = T.Func (c, [| |]) in
  let d = T.Func (d, [| |]) in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let z = T.Var 2 in
  (* x != f(c, d), p(f(d, c), x), y != d, c != z *)
  let orig_clause = {
    C.cl_id = 1;
    C.cl_lits = [
      T.mk_ineq x (f c d);
      p (f d c) x;
      T.mk_ineq y d;
      T.mk_ineq c z;
    ];
  } in
  (* x != f(y, z), p(x, x), y != d, z != c *)
  let flat_clause = {
    C.cl_id = 1;
    C.cl_lits = [
      T.mk_ineq x (f y z);
      p x x;
      T.mk_ineq y d;
      T.mk_ineq z c;
    ];
  } in
  assert_equal (Some flat_clause) (C.flatten db orig_clause)

let test_flatten_deep_nesting () =
  let db = S.create_db () in
  let f = S.add_symb db "f" 2 in
  let g = S.add_symb db "g" 1 in
  let p = S.add_symb db "p" 1 in
  let c = S.add_symb db "c" 0 in
  let d = S.add_symb db "d" 0 in
  let f a b = T.Func (f, [| a; b |]) in
  let g a = T.Func (g, [| a |]) in
  let p a = T.Func (p, [| a |]) in
  let c = T.Func (c, [| |]) in
  let d = T.Func (d, [| |]) in
  let x = T.Var 0 in
  let v1 = T.Var 1 in
  let v2 = T.Var 2 in
  let v3 = T.Var 3 in
  let v4 = T.Var 4 in
  (* p(f(c, g(d))), g(c) = x *)
  let orig_clause = {
    C.cl_id = 1;
    C.cl_lits = [
      p (f c (g d));
      T.mk_eq (g c) x;
    ];
  } in
  (* v4 != d, v3 != g(v4), v2 != c, v1 != f(v2, v3), p(v1), x = g(v2) *)
  let flat_clause = {
    C.cl_id = 1;
    C.cl_lits = [
      T.mk_ineq v4 d;
      T.mk_ineq v3 (g v4);
      T.mk_ineq v2 c;
      T.mk_ineq v1 (f v2 v3);
      p v1;
      T.mk_eq x (g v2);
    ];
  } in
  assert_equal (Some flat_clause) (C.flatten db orig_clause)

let test_flatten_func_equalities () =
  let db = S.create_db () in
  let f = S.add_symb db "f" 1 in
  let g = S.add_symb db "g" 1 in
  let c = S.add_symb db "c" 0 in
  let f a = T.Func (f, [| a |]) in
  let g a = T.Func (g, [| a |]) in
  let c = T.Func (c, [| |]) in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let z = T.Var 2 in
  let v1 = T.Var 3 in
  let v2 = T.Var 4 in
  (* g(y) = f(x), f(y) = f(x), g(x) = g(z), g(z) = f(x), c = g(z) *)
  let orig_clause = {
    C.cl_id = 1;
    C.cl_lits = [
      T.mk_eq (g y) (f x);
      T.mk_eq (f y) (f x);
      T.mk_eq (g x) (g z);
      T.mk_eq (g z) (f x);
      T.mk_eq c (g z);
    ];
  } in
  (* v2 != f(x), v1 != g(z), v2 = g(y), v2 = f(y), v1 = g(x), v1 = v2, v1 = c *)
  let flat_clause = {
    C.cl_id = 1;
    C.cl_lits = [
      T.mk_ineq v2 (f x);
      T.mk_ineq v1 (g z);
      T.mk_eq v2 (g y);
      T.mk_eq v2 (f y);
      T.mk_eq v1 (g x);
      T.mk_eq v1 v2;
      T.mk_eq v1 c;
    ];
  } in
  assert_equal (Some flat_clause) (C.flatten db orig_clause)

let test_flatten_tautology () =
  let db = S.create_db () in
  let p = S.add_symb db "p" 1 in
  let c = S.add_symb db "c" 0 in
  let p a = T.Func (p, [| a |]) in
  let c = T.Func (c, [| |]) in
  let x = T.Var 0 in
  (* x != c, p(c), ~p(x) *)
  let orig_clause = {
    C.cl_id = 1;
    C.cl_lits = [
      T.mk_ineq x c;
      p c;
      C.neg_lit (p x);
    ];
  } in
  assert_equal None (C.flatten db orig_clause)

let test_unflatten () =
  let db = S.create_db () in
  let p = S.add_symb db "p" 1 in
  let f = S.add_symb db "f" 1 in
  let c = S.add_symb db "c" 0 in
  let d = S.add_symb db "d" 0 in
  let p a = T.Func (p, [| a |]) in
  let f a = T.Func (f, [| a |]) in
  let c = T.Func (c, [| |]) in
  let d = T.Func (d, [| |]) in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let z = T.Var 2 in
  (* f(y) = x, z != c, ~p(x), z != f(y), f(d) != y *)
  let orig_clause = {
    C.cl_id = 1;
    C.cl_lits = [
      T.mk_eq (f y) x;
      T.mk_ineq z c;
      C.neg_lit (p x);
      T.mk_ineq z (f y);
      T.mk_ineq (f d) y;
    ];
  } in
  let exp_clause = {
    C.cl_id = 1;
    C.cl_lits = [
      T.mk_eq x (f (f d));
      C.neg_lit (p x);
      T.mk_ineq (f (f d)) c;
    ];
  } in
  assert_equal (Some exp_clause) (C.unflatten db orig_clause)

let test_unflatten_term_contains_var () =
  let db = S.create_db () in
  let f = S.add_symb db "f" 2 in
  let c = S.add_symb db "c" 0 in
  let d = S.add_symb db "d" 0 in
  let f a b = T.Func (f, [| a; b |]) in
  let c = T.Func (c, [| |]) in
  let d = T.Func (d, [| |]) in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let z = T.Var 2 in
  (* f(c, y) != y, z != x, c = d, y = c, x != d *)
  let orig_clause = {
    C.cl_id = 1;
    C.cl_lits = [
      T.mk_ineq (f c y) y;
      T.mk_ineq z x;
      T.mk_eq c d;
      T.mk_eq y c;
      T.mk_ineq x d;
    ];
  } in
  let exp_clause = {
    C.cl_id = 1;
    C.cl_lits = [
      T.mk_ineq y (f c y);
      T.mk_eq c d;
      T.mk_eq y c;
    ];
  } in
  assert_equal (Some exp_clause) (C.unflatten db orig_clause)

let suite =
  "Clause suite" >:::
    [
      "neg_lit 1" >:: test_neg_lit1;
      "neg_lit 2" >:: test_neg_lit2;
      "true_lit, false_lit" >:: test_true_lit_false_lit;
      "simplify" >:: test_simplify;
      "simplify tautology" >:: test_simplify_tautology;
      "normalize_vars" >:: test_normalize_vars;
      "flatten" >:: test_flatten;
      "flatten - commutative symbol" >:: test_flatten_commutative_symb;
      "flatten - deep nesting" >:: test_flatten_deep_nesting;
      "flatten - equalities of function terms" >:: test_flatten_func_equalities;
      "flatten - tautology" >:: test_flatten_tautology;
      "unflatten" >:: test_unflatten;
      "unflatten - term contains var" >:: test_unflatten_term_contains_var;
    ]
