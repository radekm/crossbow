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

let suite =
  "Earray suite" >:::
    [
      "pick from empty array" >:: test_pick_empty_array;
      "pick 1" >:: test_pick1;
      "pick 2" >:: test_pick2;
    ]
