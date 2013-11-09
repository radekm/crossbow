(* Copyright (c) 2013 Radek Micek *)

open OUnit

module T = Term
module L = Lit

let (|>) = BatPervasives.(|>)

let test_detect_commutativity () =
  let db = Symb.create_db () in
  let f = Symb.add_func db 2 in
  let g = Symb.add_func db 2 in
  let h = Symb.add_func db 2 in
  let clause = [
    L.mk_eq
      (T.func (g, [| T.var 0; T.var 1 |]))
      (T.func (h, [| T.var 1; T.var 0 |]));
  ] in
  let clause2 = [
    L.mk_eq
      (T.func (f, [| T.var 1; T.var 2 |]))
      (T.func (f, [| T.var 2; T.var 1 |]));
  ] in
  let clauses = BatDynArray.of_list [clause; clause2] in
  let new_clauses = Prop_det.detect_commutativity db clauses in

  assert_equal [clause] (BatDynArray.to_list new_clauses);
  assert_bool "" (Symb.commutative db f);
  assert_bool "" (Symb.commutative db g |> not);
  assert_bool "" (Symb.commutative db h |> not)

let test_detect_commutativity2 () =
  let db = Symb.create_db () in
  let f = Symb.add_func db 2 in
  let g = Symb.add_func db 2 in
  let clause = [
    L.mk_eq
      (T.func (f, [| T.var 4; T.var 5 |]))
      (T.var 3);
    L.mk_ineq
      (T.var 3)
      (T.func (f, [| T.var 5; T.var 4 |]));
  ] in
  let clause2 = [
    L.mk_eq
      (T.func (g, [| T.var 0; T.var 1 |]))
      (T.func (g, [| T.var 1; T.var 0 |]));
    L.mk_ineq (T.var 1) (T.var 2);
  ] in
  let clauses = BatDynArray.of_list [clause; clause2] in
  let new_clauses = Prop_det.detect_commutativity db clauses in

  assert_equal [] (BatDynArray.to_list new_clauses);
  assert_bool "" (Symb.commutative db f);
  assert_bool "" (Symb.commutative db g)

let test_detect_commutativity3 () =
  let db = Symb.create_db () in
  let f = Symb.add_func db 2 in
  let c = Symb.add_func db 0 in
  let clause = [
    L.mk_eq
      (T.func (f, [| T.func (c, [| |]); T.var 0 |]))
      (T.func (f, [| T.var 0; T.func (c, [| |]) |]));
  ] in
  let clauses = BatDynArray.of_list [clause] in
  let new_clauses = Prop_det.detect_commutativity db clauses in

  assert_equal [clause] (BatDynArray.to_list new_clauses);
  assert_bool "" (Symb.commutative db f |> not)

let test_detect_hints_for_groups () =
  let db = Symb.create_db () in
  let esymb = Symb.add_func db 0 in
  let e = T.func (esymb, [| |]) in
  let gsymb = Symb.add_func db 1 in
  let g a = T.func (gsymb, [| a |]) in
  let fsymb = Symb.add_func db 2 in
  let f a b = T.func (fsymb, [| a; b |]) in
  let x = T.var 1 in
  let y = T.var 2 in
  let z = T.var 0 in
  let clauses =
    BatDynArray.of_list
      [
        [ L.mk_eq e (g e) ];
        [ L.mk_eq (f (f x y) z) (f x (f y z)) ];
        [ L.mk_eq (f x e) x ];
        [ L.mk_eq (f e x) x ];
        [ L.mk_eq x (g (g x)) ];
        [ L.mk_eq (f (g x) x) e ];
      ] in

  Prop_det.detect_hints_for_groups db clauses;
  assert_equal [] (Symb.hints db esymb);
  assert_equal [] (Symb.hints db gsymb);
  assert_equal [] (Symb.hints db fsymb);

  (* Add missing axiom. *)
  BatDynArray.add clauses [ L.mk_eq e (f x (g x)) ];
  Prop_det.detect_hints_for_groups db clauses;
  assert_equal [] (Symb.hints db esymb);
  assert_equal [Symb.Permutation] (Symb.hints db gsymb);
  assert_equal [Symb.Latin_square] (Symb.hints db fsymb)

let test_detect_hints_for_quasigroups () =
  let db = Symb.create_db () in
  let multsymb = Symb.add_func db 2 in
  let mult a b = T.func (multsymb, [| a; b |]) in
  let ldsymb = Symb.add_func db 2 in
  let ld a b = T.func (ldsymb, [| a; b |]) in
  let rdsymb = Symb.add_func db 2 in
  let rd a b = T.func (rdsymb, [| a; b |]) in
  let gsymb = Symb.add_func db 2 in
  let g a b = T.func (gsymb, [| a; b |]) in
  let x = T.var 1 in
  let y = T.var 2 in
  let clauses =
    BatDynArray.of_list
      [
        (* Quasigroup: mult, ld, rd. *)
        [ L.mk_eq (mult y (ld y x)) x ];
        [ L.mk_eq x (mult (rd x y) y) ];
        [ L.mk_eq x (rd (mult x y) y) ];
        [ L.mk_eq (ld y (mult y x)) x ];
        (* Quasigroup: rd, g, mult. *)
        [ L.mk_eq (rd y (g y x)) x ];
        [ L.mk_eq (g y (rd y x)) x ];
      ] in

  Prop_det.detect_hints_for_quasigroups db clauses;
  assert_equal [Symb.Latin_square] (Symb.hints db multsymb);
  assert_equal [] (Symb.hints db ldsymb);
  assert_equal [Symb.Latin_square] (Symb.hints db rdsymb);
  assert_equal [] (Symb.hints db gsymb)

let test_detect_hints_for_involutive_funcs () =
  let db = Symb.create_db () in
  let f = Symb.add_func db 1 in
  let g = Symb.add_func db 1 in
  let h = Symb.add_func db 1 in
  let i = Symb.add_func db 1 in
  let clause = [
    L.mk_eq
      (T.func (f, [| T.func (g, [| T.var 1 |]) |]))
      (T.var 1);
  ] in
  let clause2 = [
    L.mk_eq
      (T.var 1)
      (T.func (h, [| T.var 0 |]));
    L.mk_ineq
      (T.func (h, [| T.var 1 |]))
      (T.var 0);
  ] in
  let clause3 = [
    L.mk_eq
      (T.var 0)
      (T.func (i, [| T.func (i, [| T.func (i, [| T.var 0 |]) |]) |]));
  ] in
  let clauses = BatDynArray.of_list [clause; clause2; clause3] in
  Prop_det.detect_hints_for_involutive_funcs db clauses;

  assert_equal [] (Symb.hints db f);
  assert_equal [] (Symb.hints db g);
  assert_equal [Symb.Permutation] (Symb.hints db h);
  assert_equal [] (Symb.hints db i)

let suite =
  "Prop_det suite" >:::
    [
      "detect_commutativity" >:: test_detect_commutativity;
      "detect_commutativity 2" >:: test_detect_commutativity2;
      "detect_commutativity 3" >:: test_detect_commutativity3;
      "detect_hints_for_groups" >:: test_detect_hints_for_groups;
      "detect_hints_for_quasigroups" >:: test_detect_hints_for_quasigroups;
      "detect_hints_for_involutive_funcs" >::
        test_detect_hints_for_involutive_funcs;
    ]
