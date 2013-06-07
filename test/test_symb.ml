
open OUnit

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

let suite =
  "Symb suite" >:::
    [
      "iter" >:: test_iter;
      "set_commutative" >:: test_set_commutative;
      "set_commutative rejects predefined symbols" >::
        test_set_commutative_rejects_predefined_symbs;
      "set_commutative rejects non-binary symbols" >::
        test_set_commutative_rejects_nonbinary_symbs;
      "set_auxiliary" >:: test_set_auxiliary;
      "set_auxiliary rejects predefined symbols" >::
        test_set_auxiliary_rejects_predefined_symbs;
    ]
