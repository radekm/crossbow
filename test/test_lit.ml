(* Copyright (c) 2013 Radek Micek *)

open OUnit

module S = Symb
module T = Term
module L = Lit

let test_mk_eq () =
  let Symb.Wr db = Symb.create_db () in
  let f = Symb.add_func db 2 in
  let l = T.Func (f, [| T.Var 1; T.Var 2 |]) in
  let r = T.Var 0 in
  assert_equal
    (L.Lit (Sh.Pos, Symb.sym_eq, [| l; r |]))
    (L.mk_eq l r)

let test_mk_ineq () =
  let Symb.Wr db = Symb.create_db () in
  let c = Symb.add_func db 0 in
  let d = Symb.add_func db 0 in
  let c = T.Func (c, [| |]) in
  let d = T.Func (d, [| |]) in
  assert_equal
    (L.Lit (Sh.Neg, Symb.sym_eq, [| c; d |]))
    (L.mk_ineq c d)

let test_neg1 () =
  assert_equal
    (L.mk_eq (T.Var 0) (T.Var 1))
    (L.neg (L.mk_ineq (T.Var 0) (T.Var 1)))

let test_neg2 () =
  let S.Wr db = S.create_db () in
  let p = S.add_pred db 1 in
  let lit = L.Lit (Sh.Pos, p, [| T.Var 2 |]) in
  let lit' = L.Lit (Sh.Neg, p, [| T.Var 2 |]) in
  assert_equal
    lit'
    (L.neg lit);
  assert_equal
    lit
    (L.neg lit')

let test_is_true_is_false () =
  let S.Wr db = S.create_db () in
  let f = S.add_func db 1 in
  let mk_term v = T.Func (f, [| T.Var v |]) in
  (* is_true *)
  assert_bool "" (L.is_true (L.mk_eq (mk_term 1) (mk_term 1)));
  assert_bool "" (not (L.is_true (L.mk_eq (mk_term 2) (mk_term 1))));
  assert_bool "" (not (L.is_true (L.mk_ineq (mk_term 1) (mk_term 1))));
  (* is_false *)
  assert_bool "" (L.is_false (L.mk_ineq (mk_term 1) (mk_term 1)));
  assert_bool "" (not (L.is_false (L.mk_ineq (mk_term 2) (mk_term 1))));
  assert_bool "" (not (L.is_false (L.mk_eq (mk_term 1) (mk_term 1))))

let test_replace () =
  let Symb.Wr db = Symb.create_db () in
  let f = Symb.add_func db 2 in
  let g = Symb.add_func db 1 in
  let c = Symb.add_func db 0 in
  let sub_old = T.Func (f, [| T.Func (c, [| |]); T.Var 0 |]) in
  let sub_new = T.Func (g, [| T.Func (c, [| |]) |]) in
  let mk_term sub =
    L.mk_ineq
      (T.Func (g,
               [|
                 T.Func (f,
                         [|
                           sub;
                           T.Func (f, [| T.Func (c, [| |]); T.Var 1 |]);
                         |]
                 );
               |]))
      (T.Func (g, [| sub |])) in
  assert_equal
    (mk_term sub_new)
    (L.replace sub_old sub_new (mk_term sub_old))

let suite =
  "Lit suite" >:::
    [
      "mk_eq" >:: test_mk_eq;
      "mk_ineq" >:: test_mk_ineq;
      "neg 1" >:: test_neg1;
      "neg 2" >:: test_neg2;
      "is_true, is_false" >:: test_is_true_is_false;
      "replace" >:: test_replace;
    ]
