
open OUnit

let test_new_db_contains_predef_symbs () =
  let db = Symb.create_db () in
  assert_equal Symb.sym_eq (Symb.find db "=" 2);
  assert_equal Symb.sym_not (Symb.find db "~" 1)

let test_add_symb () =
  let db = Symb.create_db () in
  let s = Symb.add_symb db "+" 2 in
  assert_equal s (Symb.find db "+" 2)

let test_add_symb_rejects_empty_name () =
  let db = Symb.create_db () in
  assert_raises
    (Invalid_argument "name")
    (fun () -> Symb.add_symb db "" 1)

let test_allow_symbs_with_same_name_diff_arities () =
  let db = Symb.create_db () in
  let a1 = Symb.add_symb db "f" 1 in
  let a2 = Symb.add_symb db "f" 2 in
  let a3 = Symb.add_symb db "f" 3 in
  assert_bool "" (a1 <> a2);
  assert_bool "" (a1 <> a3);
  assert_bool "" (a2 <> a3)

let test_reject_duplicate_symb () =
  let db = Symb.create_db () in
  let _ = Symb.add_symb db "p" 2 in
  assert_raises
    (Failure "duplicate symbol")
    (fun () -> Symb.add_symb db "p" 2)

let test_iter () =
  let db = Symb.create_db () in
  let s1 = Symb.add_symb db "+" 2 in
  let s2 = Symb.add_symb db "-" 1 in
  let s3 = Symb.add_symb db "0" 0 in
  let s4 = Symb.add_anon_symb db 3 in
  let symbs = ref [s1; s2; s3; s4; Symb.sym_eq; Symb.sym_not] in
  Symb.iter
    (fun s ->
      assert_bool "" (List.mem s !symbs);
      symbs := BatList.remove !symbs s)
    db;
  assert_equal [] !symbs

let test_allow_anon_symbs_with_same_arity () =
  let db = Symb.create_db () in
  let s1 = Symb.add_anon_symb db 2 in
  let s2 = Symb.add_anon_symb db 2 in
  assert_bool "" (s1 <> s2)

let test_mem_ignores_anon_symbs () =
  let db = Symb.create_db () in
  let _ = Symb.add_anon_symb db 2 in
  assert_bool "" (not (Symb.mem db "" 2))

let test_set_commutative () =
  let db = Symb.create_db () in
  let s1 = Symb.add_symb db "f" 2 in
  let s2 = Symb.add_anon_symb db 2 in
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
  let db = Symb.create_db () in
  assert_raises
    (Failure "predefined symbol")
    (fun () -> Symb.set_commutative db Symb.sym_eq true)

let test_set_commutative_rejects_nonbinary_symbs () =
  let db = Symb.create_db () in
  let s = Symb.add_symb db "ite" 3 in
  assert_raises
    (Failure "non-binary symbol")
    (fun () -> Symb.set_commutative db s true)

let test_set_auxiliary () =
  let db = Symb.create_db () in
  let s = Symb.add_anon_symb db 2 in
  assert_bool "" (not (Symb.auxiliary db s));
  Symb.set_auxiliary db s true;
  assert_bool "" (Symb.auxiliary db s)

let test_set_auxiliary_rejects_predefined_symbs () =
  let db = Symb.create_db () in
  assert_raises
    (Failure "predefined symbol")
    (fun () -> Symb.set_auxiliary db Symb.sym_not true)

let suite =
  "Symb suite" >:::
    [
      "predefined symbols are present in a new database" >::
        test_new_db_contains_predef_symbs;
      "add_symb" >:: test_add_symb;
      "add_symb rejects empty name" >:: test_add_symb_rejects_empty_name;
      "allow symbols with the same name and different arities" >::
        test_allow_symbs_with_same_name_diff_arities;
      "reject duplicate symbol" >:: test_reject_duplicate_symb;
      "iter" >:: test_iter;
      "allow anonymous symbols with the same arity" >::
        test_allow_anon_symbs_with_same_arity;
      "mem ignores anonymous symbols" >:: test_mem_ignores_anon_symbs;
      "set_commutative" >:: test_set_commutative;
      "set_commutative rejects predefined symbols" >::
        test_set_commutative_rejects_predefined_symbs;
      "set_commutative rejects non-binary symbols" >::
        test_set_commutative_rejects_nonbinary_symbs;
      "set_auxiliary" >:: test_set_auxiliary;
      "set_auxiliary rejects predefined symbols" >::
        test_set_auxiliary_rejects_predefined_symbs;
    ]
