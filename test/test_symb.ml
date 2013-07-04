
open OUnit

let test_ids_differ () =
  let Symb.Wr db = Symb.create_db () in
  let s1 = Symb.add_func db 2 in
  let s2 = Symb.add_func db 2 in
  let s3 = Symb.add_pred db 2 in
  let s4 = Symb.add_func db 1 in
  let s5 = Symb.add_pred db 1 in
  let s6 = Symb.add_pred db 1 in
  let s7 = Symb.add_func db 0 in
  let s8 = Symb.add_pred db 0 in
  let s9 = Symb.add_func db 0 in
  let all = [Symb.sym_eq; s1; s2; s3; s4; s5; s6; s7; s8; s9] in
  assert_equal (List.length all) (List.length (BatList.unique all))

let test_ids_order () =
  let Symb.Wr db = Symb.create_db () in
  let s1 = Symb.add_func db 2 in
  let s2 = Symb.add_pred db 1 in
  let s3 = Symb.add_pred db 2 in
  let s4 = Symb.add_func db 1 in
  let s5 = Symb.add_func db 3 in
  assert_bool "" (Symb.sym_eq < s1);
  assert_bool "" (s1 < s2);
  assert_bool "" (s2 < s3);
  assert_bool "" (s3 < s4);
  assert_bool "" (s4 < s5)

let test_arity_kind () =
  let Symb.Wr db = Symb.create_db () in
  let s1 = Symb.add_pred db 3 in
  let s2 = Symb.add_func db 0 in
  let s3 = Symb.add_func db Symb.max_arity in
  let s4 = Symb.add_pred db Symb.max_arity in
  let s5 = Symb.add_func db 2 in
  let s6 = Symb.add_pred db 0 in
  (* Arity. *)
  assert_equal 3 (Symb.arity s1);
  assert_equal 0 (Symb.arity s2);
  assert_equal Symb.max_arity (Symb.arity s3);
  assert_equal Symb.max_arity (Symb.arity s4);
  assert_equal 2 (Symb.arity s5);
  assert_equal 0 (Symb.arity s6);
  (* Kind. *)
  assert_equal Symb.Pred (Symb.kind s1);
  assert_equal Symb.Func (Symb.kind s2);
  assert_equal Symb.Func (Symb.kind s3);
  assert_equal Symb.Pred (Symb.kind s4);
  assert_equal Symb.Func (Symb.kind s5);
  assert_equal Symb.Pred (Symb.kind s6)

let test_iter () =
  let Symb.Wr db = Symb.create_db () in
  let s1 = Symb.add_func db 2 in
  let s2 = Symb.add_pred db 1 in
  let s3 = Symb.add_func db 0 in
  let s4 = Symb.add_func db 3 in
  let symbs = ref [s1; s2; s3; s4; Symb.sym_eq] in
  Symb.iter
    (fun s ->
      assert_bool "" (List.mem s !symbs);
      symbs := BatList.remove !symbs s)
    db;
  assert_equal [] !symbs

let test_set_commutative () =
  let Symb.Wr db = Symb.create_db () in
  let s1 = Symb.add_func db 2 in
  let s2 = Symb.add_func db 2 in
  assert_bool "" (not (Symb.commutative db s1));
  assert_bool "" (not (Symb.commutative db s2));
  Symb.set_commutative db s1 true;
  assert_bool "" (Symb.commutative db s1);
  assert_bool "" (not (Symb.commutative db s2));
  Symb.set_commutative db s2 true;
  assert_bool "" (Symb.commutative db s1);
  assert_bool "" (Symb.commutative db s2);
  Symb.set_commutative db s2 false;
  assert_bool "" (Symb.commutative db s1);
  assert_bool "" (not (Symb.commutative db s2))

let test_set_commutative_rejects_predefined_symbs () =
  let Symb.Wr db = Symb.create_db () in
  assert_raises
    (Failure "predefined symbol")
    (fun () -> Symb.set_commutative db Symb.sym_eq true)

let test_set_commutative_rejects_nonbinary_symbs () =
  let Symb.Wr db = Symb.create_db () in
  let s = Symb.add_pred db 3 in
  assert_raises
    (Failure "non-binary symbol")
    (fun () -> Symb.set_commutative db s true)

let test_set_auxiliary () =
  let Symb.Wr db = Symb.create_db () in
  let s = Symb.add_func db 2 in
  assert_bool "" (not (Symb.auxiliary db s));
  Symb.set_auxiliary db s true;
  assert_bool "" (Symb.auxiliary db s)

let test_set_auxiliary_rejects_predefined_symbs () =
  let Symb.Wr db = Symb.create_db () in
  assert_raises
    (Failure "predefined symbol")
    (fun () -> Symb.set_auxiliary db Symb.sym_eq true)

let test_add_hint () =
  let Symb.Wr db = Symb.create_db () in
  let p = Symb.add_pred db 2 in
  let f = Symb.add_func db 2 in
  let g = Symb.add_func db 1 in
  let c = Symb.add_func db 0 in
  Symb.add_hint db g Symb.Permutation;
  Symb.add_hint db f Symb.Latin_square;
  assert_equal [] (Symb.hints db p);
  assert_equal [Symb.Latin_square] (Symb.hints db f);
  assert_equal [Symb.Permutation] (Symb.hints db g);
  assert_equal [] (Symb.hints db c)

let test_add_hint_rejects_predefined_symbs () =
  let Symb.Wr db = Symb.create_db () in
  assert_raises
    (Failure "predefined symbol")
    (fun () -> Symb.add_hint db Symb.sym_eq Symb.Latin_square)

let test_add_hint_rejects_incompatible_symbs () =
  let Symb.Wr db = Symb.create_db () in
  let p = Symb.add_pred db 2 in
  let f = Symb.add_func db 2 in
  let c = Symb.add_func db 0 in
  assert_raises
    (Failure "kind")
    (fun () -> Symb.add_hint db p Symb.Latin_square);
  assert_raises
    (Failure "latin square")
    (fun () -> Symb.add_hint db c Symb.Latin_square);
  assert_raises
    (Failure "permutation")
    (fun () -> Symb.add_hint db f Symb.Permutation)

let suite =
  "Symb suite" >:::
    [
      "ids differ" >:: test_ids_differ;
      "order of ids" >:: test_ids_order;
      "arity, kind" >:: test_arity_kind;
      "iter" >:: test_iter;
      "set_commutative" >:: test_set_commutative;
      "set_commutative rejects predefined symbols" >::
        test_set_commutative_rejects_predefined_symbs;
      "set_commutative rejects non-binary symbols" >::
        test_set_commutative_rejects_nonbinary_symbs;
      "set_auxiliary" >:: test_set_auxiliary;
      "set_auxiliary rejects predefined symbols" >::
        test_set_auxiliary_rejects_predefined_symbs;
      "add_hint" >:: test_add_hint;
      "add_hint rejects predefined symbols" >::
        test_add_hint_rejects_predefined_symbs;
      "add_hint rejects incompatible symbols" >::
        test_add_hint_rejects_incompatible_symbs;
    ]
