(* Copyright (c) 2013 Radek Micek *)

open OUnit

let test_create () =
  let arity = 5 in
  let adeq_sizes = [| 3; 8; 0; 5; 6; 0; 2 |] in
  let max_size = 6 in
  let i = Sinterp.create arity adeq_sizes max_size in
  assert_equal [| 3; 6; 6; 5; 6 |] i.Sinterp.params;
  assert_equal 3240 (Array.length i.Sinterp.values);
  Array.iter (assert_equal 0) i.Sinterp.values

let test_create_zero_arity () =
  let i = Sinterp.create 0 [| |] 4 in
  assert_equal [| |] i.Sinterp.params;
  assert_equal [| 0 |] i.Sinterp.values

let test_get_set () =
  let arity = 6 in
  let adeq_sizes = [| 5; 0; 6; 8; 7; 1; 0 |] in
  let max_size = 7 in
  let i = Sinterp.create arity adeq_sizes max_size in
  let j = ref 0 in
  Assignment.each (Array.make arity 0) 0 arity adeq_sizes max_size
    (fun a -> Sinterp.set i a !j; incr j);
  j := 0;
  Assignment.each (Array.make arity 0) 0 arity adeq_sizes max_size
    (fun a -> assert_equal !j (Sinterp.get i a); incr j);
  Array.iteri assert_equal i.Sinterp.values

let test_get_set_zero_arity () =
  let i = Sinterp.create 0 [| |] 5 in
  assert_equal 0 (Sinterp.get i [| |]);
  Sinterp.set i [| |] 2;
  assert_equal 2 (Sinterp.get i [| |])

let suite =
  "Sinterp suite" >:::
    [
      "create" >:: test_create;
      "create - zero arity" >:: test_create_zero_arity;
      "get, set" >:: test_get_set;
      "get, set - zero arity" >:: test_get_set_zero_arity;
    ]
