(* Copyright (c) 2013 Radek Micek *)

open OUnit
open BatPervasives

(** Asserts that the given list of blocks satisfies the following conditions:
   - No block is empty.
   - Elements in one block are same.
   - Elements in different blocks are different.
*)
let assert_partition (partition : 'a list list) : unit =
  let representatives = BatList.map BatList.unique partition in
  (* Elements in one block of the partition are same and no block is empty. *)
  List.iter (fun xs -> assert_equal 1 (List.length xs)) representatives;
  (* Elements in different blocks of the partition are different. *)
  assert_equal
    (List.length representatives)
    (BatList.unique representatives |> List.length)
