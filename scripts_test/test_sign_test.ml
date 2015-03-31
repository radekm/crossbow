(* Copyright (c) 2015 Radek Micek *)

open OUnit2

module T = Sign_test

let assert_z_eq a b =
  assert_equal ~printer:Z.to_string a b

let assert_float_eq a b =
  assert_equal ~cmp:BatFloat.approx_equal ~printer:string_of_float a b

let test_comb test_ctx =
  assert_z_eq Z.one (T.Utils.comb 10 0);
  assert_z_eq Z.one (T.Utils.comb 10 10);
  assert_z_eq (Z.of_int 11) (T.Utils.comb 11 1);
  assert_z_eq (Z.of_int 252) (T.Utils.comb 10 5);
  assert_z_eq (Z.of_string "47129212243960") (T.Utils.comb 50 30);
  assert_z_eq
    (Z.of_string
       ("26684506508793532739261366051243" ^
        "1960190596778605587379145409383500"))
    (T.Utils.comb 250 76)

let test_binom test_ctx =
  assert_z_eq (Z.of_string "4294967296") (T.Utils.binom 32 0);
  assert_z_eq (Z.of_int 1) (T.Utils.binom 32 32);
  assert_z_eq (Z.of_string "3832555763") (T.Utils.binom 32 13);
  assert_z_eq (Z.of_int 29) (T.Utils.binom 7 5)

let test_test test_ctx =
  let test ~nneg ~nzero ~npos (lt, neq, gt) =
    (* Test all 3 hypotheses. *)
    let run ~nneg ~nzero ~npos (lt, neq, gt) =
      assert_float_eq lt (T.test ~nneg ~npos ~nzero ~ha:T.Lower);
      assert_float_eq neq (T.test ~nneg ~npos ~nzero ~ha:T.Not_equal);
      assert_float_eq gt (T.test ~nneg ~npos ~nzero ~ha:T.Greater) in
    run ~nneg ~nzero ~npos (lt, neq, gt);
    (* Swap pos with neg and gt with lt. *)
    run ~nneg:npos ~nzero ~npos:nneg (gt, neq, lt) in

  test ~nneg:3 ~nzero:5 ~npos:16 (0.590816, 0.089563, 0.044782);
  test ~nneg:3 ~nzero:2 ~npos:16 (0.748209, 0.019907, 0.009954);
  (* No ties - standard sign test. *)
  test ~nneg:7 ~nzero:0 ~npos:17 (0.988672, 0.063915, 0.031957);
  (* Same as above but single tie. *)
  test ~nneg:7 ~nzero:1 ~npos:17 (0.757795, 0.082256, 0.041128);
  (* Many ties. *)
  test ~nneg:12 ~nzero:11 ~npos:7 (0.459710, 0.919419, 0.510562)

let suite =
  "Report suite" >:::
    [
      "comb" >:: test_comb;
      "binom" >:: test_binom;
      "test" >:: test_test;
    ]
