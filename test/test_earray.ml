(* Copyright (c) 2013 Radek Micek *)

open OUnit

let test_pick_empty_array () =
  assert_equal
    None
    (Earray.pick (fun x -> Some x) [| |])

let test_pick1 () =
  assert_equal
    None
    (Earray.pick (fun x -> if x > 5 then Some x else None) [| 1; 2; 5 |])

let test_pick2 () =
  assert_equal
    (Some 7)
    (Earray.pick (fun x -> if x > 5 then Some (x+1) else None) [| 5; 6; 8 |])

let test_iter_combinations1 () =
  let arr = [| 1; 1; 2; 3; 5 |] in
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
    (fun comb -> assert_equal combs.(!i) comb; incr i) 3 arr;
  assert_equal 10 !i

let test_iter_combinations2 () =
  let arr = Array.init 9 (fun i -> i) in
  let i = ref 0 in
  Earray.iter_combinations (fun _ -> incr i) 5 arr;
  assert_equal 126 !i

let test_iter_combinations_min_len () =
  let arr = Array.init 12 (fun i -> i) in
  let i = ref 0 in
  Earray.iter_combinations (fun comb -> assert_equal [| |] comb; incr i) 0 arr;
  assert_equal 1 !i

let test_iter_combinations_max_len () =
  let arr = Array.init 7 (fun i -> i) in
  let i = ref 0 in
  Earray.iter_combinations (fun comb -> assert_equal arr comb; incr i) 7 arr;
  assert_equal 1 !i

let test_iter_combinations_bad_len () =
  let arr = Array.init 8 (fun i -> i) in
  assert_raises
    (Invalid_argument "k")
    (fun () -> Earray.iter_combinations (fun _ -> assert_bool "" false) ~-1 arr);
  assert_raises
    (Invalid_argument "k")
    (fun () -> Earray.iter_combinations (fun _ -> assert_bool "" false) 9 arr)

let suite =
  "Earray suite" >:::
    [
      "pick from empty array" >:: test_pick_empty_array;
      "pick 1" >:: test_pick1;
      "pick 2" >:: test_pick2;
      "iter_combinations 1" >:: test_iter_combinations1;
      "iter_combinations 2" >:: test_iter_combinations2;
      "iter_combinations - min length" >:: test_iter_combinations_min_len;
      "iter_combinations - max length" >:: test_iter_combinations_max_len;
      "iter_combinations - bad length" >:: test_iter_combinations_bad_len;
    ]
