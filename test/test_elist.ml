(** Copyright (c) 2013 Radek Micek *)

open OUnit

let test_contains1 () =
  assert_bool "" (Elist.contains 3 [1; 0; 4; 3; 5])

let test_contains2 () =
  assert_bool "" (not (Elist.contains 3 [1; 0; 4; -3; 5]))

let test_existsi () =
  assert_bool "" (Elist.existsi (fun i x -> i = x) [1; 3; -5; 5; 4; 9])

let test_pick1 () =
  assert_equal
    None
    (Elist.pick (fun x -> if x > 3 then Some (x-1) else None) [3; 2; -7; 3])

let test_pick2 () =
  assert_equal
    (Some 4)
    (Elist.pick (fun x -> if x > 3 then Some (x-1) else None) [3; 2; 5; 4; 3])

let test_picki1 () =
  assert_equal
    (Some 3)
    (Elist.picki (fun i x -> if 2*i = x then Some i else None) [3; 6; 7; 6; 8])

let test_picki2 () =
  assert_equal
    (Some "yes")
    (Elist.picki (fun i x -> if i + x = 3 then Some "yes" else None) [0; 2; 2])

let test_picki3 () =
  assert_equal
    None
    (Elist.picki (fun i x -> if i * x = 12 then Some x else None) [6; -2; 3; 0])

let test_pick_and_remove1 () =
  let xs = [13; 8; 5; 3; 2] in
  assert_equal
    (None, xs)
    (Elist.pick_and_remove (fun x -> if x * x < x + x then Some x else None) xs)

let test_pick_and_remove2 () =
  let xs = [15; 52; 203; 877; 4140; 21147] in
  let find_squares_to_sum x =
    let rec loop i j =
      if i*i + j*j = x then
        Some (i, j)
      else if j < x then
        loop i (j+1)
      else if i < x then
        loop (i+1) (i+1)
      else
        None in
    loop 1 1 in
  assert_equal
    (Some (4, 6), BatList.remove xs 52)
    (Elist.pick_and_remove find_squares_to_sum xs)

let suite =
  "Elist suite" >:::
    [
      "contains 1" >:: test_contains1;
      "contains 2" >:: test_contains2;
      "existsi" >:: test_existsi;
      "pick 1" >:: test_pick1;
      "pick 2" >:: test_pick2;
      "picki 1" >:: test_picki1;
      "picki 2" >:: test_picki2;
      "picki 3" >:: test_picki2;
      "pick_and_remove 1" >:: test_pick_and_remove1;
      "pick_and_remove 2" >:: test_pick_and_remove2;
    ]
