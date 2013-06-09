(* Copyright (c) 2013 Radek Micek *)

open OUnit

module S = Symb
module T = Term
module L = Lit
module C = Clause

let test_simplify () =
  let S.Wr db = S.create_db () in
  let f = S.add_func db 2 in
  let g = S.add_func db 2 in
  S.set_commutative db f true;
  let f a b = T.func (f, [| a; b |]) in
  let g a b = T.func (g, [| a; b |]) in
  let x = T.var 0 in
  let y = T.var 1 in
  let z = T.var 2 in
  (* f(x, y) != f(y, z), z != y, g(x, y) != g(y, x), g(y, x) != g(x, z), z = x *)
  let orig_clause = [
    L.mk_ineq (f x y) (f x z);
    L.mk_ineq z y;
    L.mk_ineq (g x y) (g y x);
    L.mk_ineq (g y x) (g x z);
    L.mk_eq z x;
  ] in
  (* g(x, y) != g(y, x), x = y *)
  let simpl_clause = [
    L.mk_ineq (g x y) (g y x);
    L.mk_eq x y;
  ] in
  assert_equal
    (Some simpl_clause)
    (C.simplify db orig_clause)

let test_simplify_tautology () =
  let S.Wr db = S.create_db () in
  let f = S.add_func db 1 in
  let f a = T.func (f, [| a |]) in
  let x = T.var 0 in
  let y = T.var 1 in
  let z = T.var 2 in
  (* f(y) = f(x), f(z) != f(y), x != z *)
  assert_equal
    None
    (C.simplify db [
      L.mk_eq (f y) (f x);
      L.mk_ineq (f z) (f y);
      L.mk_ineq x z;
    ]);
  (* f(x) = f(z), x != y, y != z *)
  assert_equal
    None
    (C.simplify db [
      L.mk_eq (f x) (f y);
      L.mk_ineq x y;
      L.mk_ineq y z;
    ])

let test_normalize_vars () =
  let S.Wr db = S.create_db () in
  let f = S.add_func db 2 in
  let f a b = T.func (f, [| a; b |]) in
  let orig_clause = [
    L.mk_eq (f (T.var 3) (T.var 3)) (T.var 7);
    L.mk_ineq (T.var 4) (T.var 3);
    L.mk_ineq (f (T.var 8) (T.var 7)) (f (T.var 0) (T.var 3));
  ] in
  let norm_clause = [
    L.mk_eq (f (T.var 0) (T.var 0)) (T.var 1);
    L.mk_ineq (T.var 2) (T.var 0);
    L.mk_ineq (f (T.var 3) (T.var 1)) (f (T.var 4) (T.var 0));
  ] in
  assert_equal
    (norm_clause, 5)
    (C.normalize_vars orig_clause)

let test_flatten () =
  let S.Wr db = S.create_db () in
  let f = S.add_func db 1 in
  let c = S.add_func db 0 in
  let d = S.add_func db 0 in
  let f a = T.func (f, [| a |]) in
  let c = T.func (c, [| |]) in
  let d = T.func (d, [| |]) in
  let x = T.var 0 in
  let y = T.var 1 in
  let v1 = T.var 2 in
  let v2 = T.var 3 in
  (* x != f(c), y != f(d), f(c) = f(d) *)
  let orig_clause = [
    L.mk_ineq x (f c);
    L.mk_ineq y (f d);
    L.mk_eq (f c) (f d);
  ] in
  (* v2 != d, v1 != c, x != f(v1), y != f(v2), x = y *)
  let flat_clause = [
    L.mk_ineq v2 d;
    L.mk_ineq v1 c;
    L.mk_ineq x (f v1);
    L.mk_ineq y (f v2);
    L.mk_eq x y;
  ] in
  assert_equal (Some flat_clause) (C.flatten db orig_clause)

let test_flatten_commutative_symb () =
  let S.Wr db = S.create_db () in
  let f = S.add_func db 2 in
  S.set_commutative db f true;
  let p = S.add_pred db 2 in
  let c = S.add_func db 0 in
  let d = S.add_func db 0 in
  let f a b = T.func (f, [| a; b |]) in
  let p a b = L.lit (Sh.Pos, p, [| a; b |]) in
  let c = T.func (c, [| |]) in
  let d = T.func (d, [| |]) in
  let x = T.var 0 in
  let y = T.var 1 in
  let z = T.var 2 in
  (* x != f(c, d), p(f(d, c), x), y != d, c != z *)
  let orig_clause = [
    L.mk_ineq x (f c d);
    p (f d c) x;
    L.mk_ineq y d;
    L.mk_ineq c z;
  ] in
  (* x != f(y, z), p(x, x), y != d, z != c *)
  let flat_clause = [
    L.mk_ineq x (f y z);
    p x x;
    L.mk_ineq y d;
    L.mk_ineq z c;
  ] in
  assert_equal (Some flat_clause) (C.flatten db orig_clause)

let test_flatten_deep_nesting () =
  let S.Wr db = S.create_db () in
  let f = S.add_func db 2 in
  let g = S.add_func db 1 in
  let p = S.add_pred db 1 in
  let c = S.add_func db 0 in
  let d = S.add_func db 0 in
  let f a b = T.func (f, [| a; b |]) in
  let g a = T.func (g, [| a |]) in
  let p a = L.lit (Sh.Pos, p, [| a |]) in
  let c = T.func (c, [| |]) in
  let d = T.func (d, [| |]) in
  let x = T.var 0 in
  let v1 = T.var 1 in
  let v2 = T.var 2 in
  let v3 = T.var 3 in
  let v4 = T.var 4 in
  (* p(f(c, g(d))), g(c) = x *)
  let orig_clause = [
    p (f c (g d));
    L.mk_eq (g c) x;
  ] in
  (* v4 != d, v3 != g(v4), v2 != c, v1 != f(v2, v3), p(v1), x = g(v2) *)
  let flat_clause = [
    L.mk_ineq v4 d;
    L.mk_ineq v3 (g v4);
    L.mk_ineq v2 c;
    L.mk_ineq v1 (f v2 v3);
    p v1;
    L.mk_eq x (g v2);
  ] in
  assert_equal (Some flat_clause) (C.flatten db orig_clause)

let test_flatten_func_equalities () =
  let S.Wr db = S.create_db () in
  let f = S.add_func db 1 in
  let g = S.add_func db 1 in
  let c = S.add_func db 0 in
  let f a = T.func (f, [| a |]) in
  let g a = T.func (g, [| a |]) in
  let c = T.func (c, [| |]) in
  let x = T.var 0 in
  let y = T.var 1 in
  let z = T.var 2 in
  let v1 = T.var 3 in
  let v2 = T.var 4 in
  (* g(y) = f(x), f(y) = f(x), g(x) = g(z), g(z) = f(x), c = g(z) *)
  let orig_clause = [
      L.mk_eq (g y) (f x);
      L.mk_eq (f y) (f x);
      L.mk_eq (g x) (g z);
      L.mk_eq (g z) (f x);
      L.mk_eq c (g z);
  ] in
  (* v2 != f(x), v1 != g(z), v2 = g(y), v2 = f(y), v1 = g(x), v1 = v2, v1 = c *)
  let flat_clause = [
    L.mk_ineq v2 (f x);
    L.mk_ineq v1 (g z);
    L.mk_eq v2 (g y);
    L.mk_eq v2 (f y);
    L.mk_eq v1 (g x);
    L.mk_eq v1 v2;
    L.mk_eq v1 c;
  ] in
  assert_equal (Some flat_clause) (C.flatten db orig_clause)

let test_flatten_tautology () =
  let S.Wr db = S.create_db () in
  let p = S.add_pred db 1 in
  let c = S.add_func db 0 in
  let p a = L.lit (Sh.Pos, p, [| a |]) in
  let c = T.func (c, [| |]) in
  let x = T.var 0 in
  (* x != c, p(c), ~p(x) *)
  let orig_clause = [
    L.mk_ineq x c;
    p c;
    L.neg (p x);
  ] in
  assert_equal None (C.flatten db orig_clause)

let test_unflatten () =
  let S.Wr db = S.create_db () in
  let p = S.add_pred db 1 in
  let f = S.add_func db 1 in
  let c = S.add_func db 0 in
  let d = S.add_func db 0 in
  let p a = L.lit (Sh.Pos, p, [| a |]) in
  let f a = T.func (f, [| a |]) in
  let c = T.func (c, [| |]) in
  let d = T.func (d, [| |]) in
  let x = T.var 0 in
  let y = T.var 1 in
  let z = T.var 2 in
  (* f(y) = x, z != c, ~p(x), z != f(y), f(d) != y *)
  let orig_clause = [
    L.mk_eq (f y) x;
    L.mk_ineq z c;
    L.neg (p x);
    L.mk_ineq z (f y);
    L.mk_ineq (f d) y;
  ] in
  let exp_clause = [
    L.mk_eq x (f (f d));
    L.neg (p x);
    L.mk_ineq (f (f d)) c;
  ] in
  assert_equal (Some exp_clause) (C.unflatten db orig_clause)

let test_unflatten_term_contains_var () =
  let S.Wr db = S.create_db () in
  let f = S.add_func db 2 in
  let c = S.add_func db 0 in
  let d = S.add_func db 0 in
  let f a b = T.func (f, [| a; b |]) in
  let c = T.func (c, [| |]) in
  let d = T.func (d, [| |]) in
  let x = T.var 0 in
  let y = T.var 1 in
  let z = T.var 2 in
  (* f(c, y) != y, z != x, c = d, y = c, x != d *)
  let orig_clause = [
    L.mk_ineq (f c y) y;
    L.mk_ineq z x;
    L.mk_eq c d;
    L.mk_eq y c;
    L.mk_ineq x d;
  ] in
  let exp_clause = [
    L.mk_ineq y (f c y);
    L.mk_eq c d;
    L.mk_eq y c;
  ] in
  assert_equal (Some exp_clause) (C.unflatten db orig_clause)

module IntSet = BatSet.IntSet

let test_vars () =
  let Symb.Wr db = Symb.create_db () in
  let f = Symb.add_func db 3 in
  let f a b c = T.func (f, [| a; b; c |]) in
  let g = Symb.add_func db 2 in
  let g a b = T.func (g, [| a; b |]) in
  let c = Symb.add_func db 0 in
  let c = T.func (c, [| |]) in
  let x1 = T.var 3 in
  let x2 = T.var 8 in
  let x3 = T.var 0 in
  let x4 = T.var 9 in
  let x5 = T.var 15 in
  let y = T.var 2 in
  let z = T.var 1 in
  let terms = [
    L.mk_ineq (f (g c x1) x3 x3) x2;
    L.mk_eq x4 (f (g x5 c) c c);
    L.mk_eq y (g z z);
  ] in
  let exp_vars =
    List.fold_right IntSet.add [3; 8; 0; 9; 15; 2; 1] IntSet.empty in
  assert_equal ~cmp:IntSet.equal exp_vars (C.vars terms)

let suite =
  "Clause suite" >:::
    [
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
      "vars" >:: test_vars;
    ]
