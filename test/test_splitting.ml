(* Copyright (c) 2013 Radek Micek *)

open OUnit

module T = Term
module L = Lit
module C = Clause

let (|>) = BatPervasives.(|>)

let get_auxiliary_symbs db =
  let aux = ref [] in
  Symb.iter
    (fun s ->
      if Symb.auxiliary db s then
        aux := s :: !aux)
    db;
  BatList.sort compare !aux

let show_clauses cs =
  cs
  |> BatList.map (fun cl -> C.show cl ^ "\n")
  |> String.concat ""
  |> (fun str -> "\n" ^ str)

let test_paradox_splitting_empty_cl () =
  let db = Symb.create_db () in
  let cl = [] in
  let clauses = Splitting.split_clause Splitting.paradox_splitting db cl in
  let exp_clauses = [ [] ] in
  assert_equal ~printer:show_clauses exp_clauses clauses

let test_paradox_mod_splitting_empty_cl () =
  let db = Symb.create_db () in
  let cl = [] in
  let clauses =
    Splitting.split_clause Splitting.paradox_mod_splitting db cl in
  let exp_clauses = [ [] ] in
  assert_equal ~printer:show_clauses exp_clauses clauses

let make_cl ()
    : Symb.db * C.t *
    (T.t -> T.t -> Lit.t) *
    (T.t -> T.t -> T.t -> T.t -> T.t -> T.t) *
    (T.var -> T.t) =
  let db = Symb.create_db () in
  let p = Symb.add_pred db 2 in
  let p a b = L.lit (Sh.Pos, p, [| a; b |]) in
  let f = Symb.add_func db 5 in
  let f a b c d e = T.func (f, [| a; b; c; d; e |]) in
  let x i = T.var i in
  let cl = [
    L.neg (p (x 0) (x 1));
    L.neg (p (x 2) (x 3));
    L.neg (p (x 4) (x 5));
    L.neg (p (x 6) (x 7));
    L.neg (p (x 8) (x 9));
    p (x 10) (x 11);
    L.mk_ineq (x 10) (f (x 0) (x 2) (x 4) (x 6) (x 8));
    L.mk_ineq (x 11) (f (x 1) (x 3) (x 5) (x 7) (x 9));
  ] in
  (db, cl, p, f, x)

(* Polymorphic function is needed. *)
type split = {
  f : Symb.db -> C.t -> C.t list;
}

let check_splitting_cl (split : split) =
  let (db, cl, p, f, x) = make_cl () in
  let clauses = split.f db cl in
  let q1, q2, q3, q4, q5 =
    match get_auxiliary_symbs db with
      | [q1; q2; q3; q4; q5] -> q1, q2, q3, q4, q5
      | _ -> assert_failure "auxiliary symbols" in
  let normalize cl = fst (C.normalize_vars cl) in
  let exp_clauses = [
    normalize [
      L.lit (Sh.Pos, q1, [| x 1; x 2; x 4; x 6; x 8; x 10 |]);
      L.neg (p (x 0) (x 1));
      L.mk_ineq (x 10) (f (x 0) (x 2) (x 4) (x 6) (x 8));
    ];
    normalize [
      L.lit (Sh.Pos, q2, [| x 0; x 2; x 3; x 4; x 5; x 6 |]);
      L.neg (L.lit (Sh.Pos, q1, [| x 0; x 1; x 2; x 3; x 4; x 5 |]));
      L.neg (p (x 1) (x 6));
    ];
    normalize [
      L.lit (Sh.Pos, q3, [| x 0; x 2; x 3; x 4; x 5; x 6 |]);
      L.neg (L.lit (Sh.Pos, q2, [| x 0; x 1; x 2; x 3; x 4; x 5 |]));
      L.neg (p (x 1) (x 6));
    ];
    normalize [
      L.lit (Sh.Pos, q4, [| x 0; x 2; x 3; x 4; x 5; x 6 |]);
      L.neg (L.lit (Sh.Pos, q3, [| x 0; x 1; x 2; x 3; x 4; x 5 |]));
      L.neg (p (x 1) (x 6));
    ];
    normalize [
      L.lit (Sh.Pos, q5, [| x 0; x 2; x 3; x 4; x 5; x 6 |]);
      L.neg (L.lit (Sh.Pos, q4, [| x 0; x 1; x 2; x 3; x 4; x 5 |]));
      L.neg (p (x 1) (x 6));
    ];
    normalize [
      L.neg (L.lit (Sh.Pos, q5, [| x 0; x 2; x 3; x 4; x 5; x 6 |]));
      p (x 2) (x 7);
      L.mk_ineq (x 7) (f (x 0) (x 3) (x 4) (x 5) (x 6));
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

let make_cl2 ()
    : Symb.db * C.t *
    (T.t -> T.t -> L.t) *
    (T.t -> T.t -> T.t -> T.t -> L.t) *
    L.t * (T.var -> T.t) =
  let db = Symb.create_db () in
  let p = Symb.add_pred db 2 in
  let p a b = L.lit (Sh.Pos, p, [| a; b |]) in
  let q = Symb.add_pred db 4 in
  let q a b c d = L.lit (Sh.Pos, q, [| a; b; c; d |]) in
  let r = Symb.add_pred db 0 in
  let r = L.lit (Sh.Pos, r, [| |]) in
  let x i = T.var i in
  let cl = [
    p (x 0) (x 1);
    p (x 0) (x 2);
    p (x 1) (x 2);
    r;
    q (x 3) (x 4) (x 5) (x 6);
  ] in
  (db, cl, p, q, r, x)

let test_paradox_splitting2 () =
  let (db, cl, p, q, r, x) = make_cl2 () in
  let clauses = Splitting.split_clause Splitting.paradox_splitting db cl in
  let q1, q2 =
    match get_auxiliary_symbs db with
      | [q1; q2] -> q1, q2
      | _ -> assert_failure "auxiliary symbols" in
  let normalize cl = fst (C.normalize_vars cl) in
  let exp_clauses = [
    normalize [
      L.lit (Sh.Pos, q1, [| x 1; x 2 |]);
      p (x 0) (x 1);
      p (x 0) (x 2);
    ];
    normalize [
      L.lit (Sh.Pos, q2, [| |]);
      L.neg (L.lit (Sh.Pos, q1, [| x 0; x 1 |]));
      p (x 0) (x 1);
    ];
    normalize [
      L.neg (L.lit (Sh.Pos, q2, [| |]));
      r;
      q (x 2) (x 3) (x 4) (x 5);
    ];
  ] in
  assert_equal ~printer:show_clauses exp_clauses clauses

let test_paradox_mod_splitting2 () =
  let (db, cl, p, q, r, x) = make_cl2 () in
  let clauses =
    Splitting.split_clause Splitting.paradox_mod_splitting db cl in
  let q1 =
    match get_auxiliary_symbs db with
      | [q1] -> q1
      | _ -> assert_failure "auxiliary symbols" in
  let normalize cl = fst (C.normalize_vars cl) in
  let exp_clauses = [
    normalize [
      L.lit (Sh.Pos, q1, [| |]);
      p (x 0) (x 1);
      p (x 0) (x 2);
      p (x 1) (x 2);
      r;
    ];
    normalize [
      L.neg (L.lit (Sh.Pos, q1, [| |]));
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
