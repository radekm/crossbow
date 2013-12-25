(* Copyright (c) 2013 Radek Micek *)

open OUnit2

let test_pick_empty_array test_ctx =
  assert_equal
    None
    (Earray.pick (fun x -> Some x) Earray.empty)

let test_pick1 test_ctx =
  assert_equal
    None
    (Earray.pick
       (fun x -> if x > 5 then Some x else None)
       (Earray.of_array [| 1; 2; 5 |]))

let test_pick2 test_ctx =
  assert_equal
    (Some 7)
    (Earray.pick
       (fun x -> if x > 5 then Some (x+1) else None)
       (Earray.of_array [| 5; 6; 8 |]))

let test_existsi1 test_ctx =
  assert_bool
    ""
    (Earray.of_array [| -1; 2; 1; 3; 6; 3 |]
     |> Earray.existsi (fun i x -> x > 3 && i <> 4)
     |> not)

let test_existsi2 test_ctx =
  assert_bool
    ""
    (Earray.of_array [| -3; 6; 3; 5; 8; 6; 7; 2 |]
     |> Earray.existsi (fun i x -> x > 4 && i > 5))

let test_rindex_of_empty_array test_ctx =
  assert_equal
    None
    (Earray.rindex_of (fun _ _ -> true) Earray.empty 0 0)

let test_rindex_of1 test_ctx =
  assert_equal
    (Some 2)
    (Earray.rindex_of
       (fun i x -> i <> 5 && x > 6)
       (Earray.of_array [| 0; 7; 8; 4; 5; 9; 1; 4; 9; 8 |]) 1 7)

let test_rindex_of2 test_ctx =
  assert_equal
    None
    (Earray.rindex_of
       (fun i x -> i <> 4 && i <> 6 && x > 5)
       (Earray.of_array [| 0; 7; 8; 4; 9; 1; 6; 5; 9; 8 |]) 3 5)

let test_iter_combinations1 test_ctx =
  let arr = Earray.of_array [| 1; 1; 2; 3; 5 |] in
  let combs = [|
    [| 1; 1; 2 |];
    [| 1; 1; 3 |];
    [| 1; 1; 5 |];
    [| 1; 2; 3 |];
    [| 1; 2; 5 |];
    [| 1; 3; 5 |];
    [| 1; 2; 3 |];
    [| 1; 2; 5 |];
    [| 1; 3; 5 |];
    [| 2; 3; 5 |];
  |] in
  let i = ref 0 in
  Earray.iter_combinations
    (fun comb -> assert_equal (Earray.of_array combs.(!i)) comb; incr i)
    3 arr;
  assert_equal 10 !i

let test_iter_combinations2 test_ctx =
  let arr = Earray.init 9 (fun i -> i) in
  let i = ref 0 in
  Earray.iter_combinations (fun _ -> incr i) 5 arr;
  assert_equal 126 !i

let test_iter_combinations_min_len test_ctx =
  let arr = Earray.init 12 (fun i -> i) in
  let i = ref 0 in
  Earray.iter_combinations
    (fun comb -> assert_equal Earray.empty comb; incr i)
    0 arr;
  assert_equal 1 !i

let test_iter_combinations_max_len test_ctx =
  let arr = Earray.init 7 (fun i -> i) in
  let i = ref 0 in
  Earray.iter_combinations (fun comb -> assert_equal arr comb; incr i) 7 arr;
  assert_equal 1 !i

let test_iter_combinations_bad_len test_ctx =
  let arr = Earray.init 8 (fun i -> i) in
  assert_raises
    (Invalid_argument "k")
    (fun () ->
      Earray.iter_combinations (fun _ -> assert_bool "" false) ~-1 arr);
  assert_raises
    (Invalid_argument "k")
    (fun () -> Earray.iter_combinations (fun _ -> assert_bool "" false) 9 arr)

let test_iter_permutations_empty_array test_ctx =
  let arr = Earray.empty in
  let i = ref 0 in
  Earray.iter_permutations (fun p -> assert_equal Earray.empty p; incr i) arr;
  assert_equal 1 !i

let test_iter_permutations1 test_ctx =
  let n = 4 in
  let arr = Earray.init n (fun i -> n-i) in
  let perms = ref [] in
  Earray.iter_permutations
    (fun p -> perms := Earray.copy p :: !perms)
    arr;
  let exp_perms = BatList.map Earray.of_array [
    [| 1; 2; 3; 4 |];
    [| 1; 2; 4; 3 |];
    [| 1; 3; 2; 4 |];
    [| 1; 3; 4; 2 |];
    [| 1; 4; 2; 3 |];
    [| 1; 4; 3; 2 |];
    [| 2; 1; 3; 4 |];
    [| 2; 1; 4; 3 |];
    [| 2; 3; 1; 4 |];
    [| 2; 3; 4; 1 |];
    [| 2; 4; 1; 3 |];
    [| 2; 4; 3; 1 |];
    [| 3; 1; 2; 4 |];
    [| 3; 1; 4; 2 |];
    [| 3; 2; 1; 4 |];
    [| 3; 2; 4; 1 |];
    [| 3; 4; 1; 2 |];
    [| 3; 4; 2; 1 |];
    [| 4; 1; 2; 3 |];
    [| 4; 1; 3; 2 |];
    [| 4; 2; 1; 3 |];
    [| 4; 2; 3; 1 |];
    [| 4; 3; 1; 2 |];
    [| 4; 3; 2; 1 |];
  ] in
  assert_equal exp_perms (List.rev !perms)

let test_iter_permutations2 test_ctx =
  let n = 7 in
  let arr = Earray.init n (fun i -> n-i) in
  let perms = ref [] in
  let assert_perm p =
    assert_equal n (Earray.length p);
    Earray.iter (fun x -> assert_bool "" (x >= 1 && x <= n)) p;
    assert_equal n (List.length (BatList.unique (Earray.to_list p))) in
  Earray.iter_permutations
    (fun p ->
      assert_perm p;
      perms := Earray.copy p :: !perms)
    arr;
  (* All permutations. *)
  assert_equal 5040 (List.length !perms);
  (* No duplicates. *)
  assert_equal 5040 (List.length (BatList.sort_unique compare !perms));
  (* Pemutations are in lexicographic order. *)
  assert_equal (List.rev !perms) (BatList.sort compare !perms)

let suite =
  "Earray suite" >:::
    [
      "pick from empty array" >:: test_pick_empty_array;
      "pick 1" >:: test_pick1;
      "pick 2" >:: test_pick2;
      "existsi 1" >:: test_existsi1;
      "existsi 2" >:: test_existsi2;
      "rindex_of - empty array" >:: test_rindex_of_empty_array;
      "rindex_of 1" >:: test_rindex_of1;
      "rindex_of 2" >:: test_rindex_of2;
      "iter_combinations 1" >:: test_iter_combinations1;
      "iter_combinations 2" >:: test_iter_combinations2;
      "iter_combinations - min length" >:: test_iter_combinations_min_len;
      "iter_combinations - max length" >:: test_iter_combinations_max_len;
      "iter_combinations - bad length" >:: test_iter_combinations_bad_len;
      "iter_permutations 1" >:: test_iter_permutations1;
      "iter_permutations 2" >:: test_iter_permutations2;
      "iter_permutations - empty array" >:: test_iter_permutations_empty_array;
    ]
