(* Copyright (c) 2013 Radek Micek *)

open OUnit

let assert_equivalence e exp_blocks =
  let ids = BatList.map (BatList.map (Equiv.find e)) exp_blocks in
  Eunit.assert_partition ids

let test_union_find () =
  let e = Equiv.create () in
  let add () = Equiv.add_item e in
  let union a b = Equiv.union e a b in
  let i1, i2, i3, i4, i5 = add (), add (), add (), add (), add () in
  assert_equivalence e [[i1]; [i2]; [i3]; [i4]; [i5]];
  union i1 i2;
  union i3 i5;
  assert_equivalence e [[i1; i2]; [i3; i5]; [i4]];
  union i2 i3;
  assert_equivalence e [[i1; i2; i3; i5]; [i4]];
  let i6 = add () in
  union i6 i4;
  assert_equivalence e [[i1; i2; i3; i5]; [i4; i6]];
  union i6 i1;
  assert_equivalence e [[i1; i2; i3; i4; i5; i6]]

let suite =
  "Equiv suite" >:::
    [
      "union, find" >:: test_union_find;
    ]
