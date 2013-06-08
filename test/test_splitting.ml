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
  let Symb.Wr db = Symb.create_db () in
  let cl = [] in
  let clauses = Splitting.split_clause Splitting.paradox_splitting db cl in
  let exp_clauses = [ [] ] in
  assert_equal ~printer:show_clauses exp_clauses clauses

let test_paradox_mod_splitting_empty_cl () =
  let Symb.Wr db = Symb.create_db () in
  let cl = [] in
  let clauses =
    Splitting.split_clause Splitting.paradox_mod_splitting db cl in
  let exp_clauses = [ [] ] in
  assert_equal ~printer:show_clauses exp_clauses clauses

type wrap_cl =
  | Wr_cl :
      's Symb.db * 's C.t *
      ('s T.t -> 's T.t -> 's T.t) *
      ('s T.t -> 's T.t -> 's T.t -> 's T.t -> 's T.t -> 's T.t) *
      (T.var -> 's T.t) ->
      wrap_cl

let make_cl () =
  let Symb.Wr db = Symb.create_db () in
  let p = Symb.add_pred db 2 in
  let p a b = T.Func (p, [| a; b |]) in
  let f = Symb.add_func db 5 in
  let f a b c d e = T.Func (f, [| a; b; c; d; e |]) in
  let x i = T.Var i in
  let cl = [
    T.neg_lit (p (x 0) (x 1));
    T.neg_lit (p (x 2) (x 3));
    T.neg_lit (p (x 4) (x 5));
    T.neg_lit (p (x 6) (x 7));
    T.neg_lit (p (x 8) (x 9));
    p (x 10) (x 11);
    T.mk_ineq (x 10) (f (x 0) (x 2) (x 4) (x 6) (x 8));
    T.mk_ineq (x 11) (f (x 1) (x 3) (x 5) (x 7) (x 9));
  ] in
  Wr_cl (db, cl, p, f, x)

(* Polymorphic function is needed. *)
type split = {
  f : 's. 's Symb.db -> 's C.t -> 's C.t list;
}

let check_splitting_cl (split : split) =
  let Wr_cl (db, cl, p, f, x) = make_cl () in
  let clauses = split.f db cl in
  let q1, q2, q3, q4, q5 =
    match get_auxiliary_symbs db with
      | [q1; q2; q3; q4; q5] -> q1, q2, q3, q4, q5
      | _ -> assert_failure "auxiliary symbols" in
  let normalize cl = fst (C.normalize_vars cl) in
  let exp_clauses = [
    normalize [
      T.Func (q1, [| x 1; x 2; x 4; x 6; x 8; x 10 |]);
      T.neg_lit (p (x 0) (x 1));
      T.mk_ineq (x 10) (f (x 0) (x 2) (x 4) (x 6) (x 8));
    ];
    normalize [
      T.Func (q2, [| x 0; x 2; x 3; x 4; x 5; x 6 |]);
      T.neg_lit (T.Func (q1, [| x 0; x 1; x 2; x 3; x 4; x 5 |]));
      T.neg_lit (p (x 1) (x 6));
    ];
    normalize [
      T.Func (q3, [| x 0; x 2; x 3; x 4; x 5; x 6 |]);
      T.neg_lit (T.Func (q2, [| x 0; x 1; x 2; x 3; x 4; x 5 |]));
      T.neg_lit (p (x 1) (x 6));
    ];
    normalize [
      T.Func (q4, [| x 0; x 2; x 3; x 4; x 5; x 6 |]);
      T.neg_lit (T.Func (q3, [| x 0; x 1; x 2; x 3; x 4; x 5 |]));
      T.neg_lit (p (x 1) (x 6));
    ];
    normalize [
      T.Func (q5, [| x 0; x 2; x 3; x 4; x 5; x 6 |]);
      T.neg_lit (T.Func (q4, [| x 0; x 1; x 2; x 3; x 4; x 5 |]));
      T.neg_lit (p (x 1) (x 6));
    ];
    normalize [
      T.neg_lit (T.Func (q5, [| x 0; x 2; x 3; x 4; x 5; x 6 |]));
      p (x 2) (x 7);
      T.mk_ineq (x 7) (f (x 0) (x 3) (x 4) (x 5) (x 6));
    ];
  ] in
  assert_equal ~printer:show_clauses exp_clauses clauses

let test_paradox_splitting () =
  let f p cl =
    Splitting.split_clause Splitting.paradox_splitting p cl in
  check_splitting_cl { f }

let test_paradox_mod_splitting () =
  let f p cl =
    Splitting.split_clause Splitting.paradox_mod_splitting p cl in
  check_splitting_cl { f }

type wrap_cl2 =
  | Wr_cl2 :
      's Symb.db * 's C.t *
      ('s T.t -> 's T.t -> 's T.t) *
      ('s T.t -> 's T.t -> 's T.t -> 's T.t -> 's T.t) *
      's T.t *
      (T.var -> 's T.t) ->
      wrap_cl2

let make_cl2 () =
  let Symb.Wr db = Symb.create_db () in
  let p = Symb.add_pred db 2 in
  let p a b = T.Func (p, [| a; b |]) in
  let q = Symb.add_pred db 4 in
  let q a b c d = T.Func (q, [| a; b; c; d |]) in
  let r = Symb.add_pred db 0 in
  let r = T.Func (r, [| |]) in
  let x i = T.Var i in
  let cl = [
    p (x 0) (x 1);
    p (x 0) (x 2);
    p (x 1) (x 2);
    r;
    q (x 3) (x 4) (x 5) (x 6);
  ] in
  Wr_cl2 (db, cl, p, q, r, x)

let test_paradox_splitting2 () =
  let Wr_cl2 (db, cl, p, q, r, x) = make_cl2 () in
  let clauses = Splitting.split_clause Splitting.paradox_splitting db cl in
  let q1, q2 =
    match get_auxiliary_symbs db with
      | [q1; q2] -> q1, q2
      | _ -> assert_failure "auxiliary symbols" in
  let normalize cl = fst (C.normalize_vars cl) in
  let exp_clauses = [
    normalize [
      T.Func (q1, [| x 1; x 2 |]);
      p (x 0) (x 1);
      p (x 0) (x 2);
    ];
    normalize [
      T.Func (q2, [| |]);
      T.neg_lit (T.Func (q1, [| x 0; x 1 |]));
      p (x 0) (x 1);
    ];
    normalize [
      T.neg_lit (T.Func (q2, [| |]));
      r;
      q (x 2) (x 3) (x 4) (x 5);
    ];
  ] in
  assert_equal ~printer:show_clauses exp_clauses clauses

let test_paradox_mod_splitting2 () =
  let Wr_cl2 (db, cl, p, q, r, x) = make_cl2 () in
  let clauses =
    Splitting.split_clause Splitting.paradox_mod_splitting db cl in
  let q1 =
    match get_auxiliary_symbs db with
      | [q1] -> q1
      | _ -> assert_failure "auxiliary symbols" in
  let normalize cl = fst (C.normalize_vars cl) in
  let exp_clauses = [
    normalize [
      T.Func (q1, [| |]);
      p (x 0) (x 1);
      p (x 0) (x 2);
      p (x 1) (x 2);
      r;
    ];
    normalize [
      T.neg_lit (T.Func (q1, [| |]));
      q (x 3) (x 4) (x 5) (x 6);
    ];
  ] in
  assert_equal ~printer:show_clauses exp_clauses clauses

let suite =
  "Splitting suite" >:::
    [
      "paradox_splitting - empty clause" >:: test_paradox_splitting_empty_cl;
      "paradox_mod_splitting - empty clause" >::
        test_paradox_mod_splitting_empty_cl;
      "paradox_splitting" >:: test_paradox_splitting;
      "paradox_mod_splitting" >:: test_paradox_mod_splitting;
      "paradox_splitting 2" >:: test_paradox_splitting2;
      "paradox_mod_splitting 2" >:: test_paradox_mod_splitting2;
    ]
