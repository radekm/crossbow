(* Copyright (c) 2013 Radek Micek *)

open OUnit
open Sorts

let print_result result =
  List.iter
    (fun ((s, args), (lo, hi)) ->
      Printf.printf "\nsymb %s (%s) %d %d\n"
        (BatPervasives.dump s)
        (String.concat ","
           (Array.to_list (Array.map string_of_int args))) lo hi)
    result

let test_no_consts_no_funcs () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 2 in
  let q = Symb.add_pred db 1 in

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 0; 1 |];
  Hashtbl.add symb_sorts q [| 1 |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 0; 5 |];
    consts =
      [|
        [| |];
        [| |];
      |];
    only_consts = ref true;
  } in

  let sr = Symred.create prob sorts in
  for i = 1 to 100 do
    assert_equal [] (Symred.incr_max_size sr)
  done

let test_consts_in_same_sort () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 2 in
  let c = Symb.add_func db 0 in
  let q = Symb.add_pred db 1 in
  let d = Symb.add_func db 0 in

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 1; 1 |];
  Hashtbl.add symb_sorts c [| 1 |];
  Hashtbl.add symb_sorts q [| 0 |];
  Hashtbl.add symb_sorts d [| 1 |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 0; 5 |];
    consts =
      [|
        [| |];
        [| c; d |];
      |];
    only_consts = ref true;
  } in

  let sr = Symred.create prob sorts in
  assert_equal [(c, [| |]), (0, 0)] (Symred.incr_max_size sr);
  assert_equal [(d, [| |]), (0, 1)] (Symred.incr_max_size sr);
  for i = 3 to 100 do
    assert_equal [] (Symred.incr_max_size sr)
  done

let test_consts_in_diff_sorts () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 2 in
  let c = Symb.add_func db 0 in
  let q = Symb.add_pred db 1 in
  let d = Symb.add_func db 0 in
  let e = Symb.add_func db 0 in

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 1; 1 |];
  Hashtbl.add symb_sorts c [| 0 |];
  Hashtbl.add symb_sorts q [| 0 |];
  Hashtbl.add symb_sorts d [| 1 |];
  Hashtbl.add symb_sorts e [| 0 |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 0; 5 |];
    consts =
      [|
        [| c; e |];
        [| d |];
      |];
    only_consts = ref true;
  } in

  let sr = Symred.create prob sorts in
  assert_equal
    [(c, [| |]), (0, 0); (d, [| |]), (0, 0)]
    (Symred.incr_max_size sr);
  assert_equal [(e, [| |]), (0, 1)] (Symred.incr_max_size sr);
  for i = 3 to 100 do
    assert_equal [] (Symred.incr_max_size sr)
  done

let test_distinct_consts () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 2 in
  let c = Symb.add_func db 0 in
  let q = Symb.add_pred db 0 in
  let d = Symb.add_func db 0 in
  let e = Symb.add_func db 0 in
  BatDynArray.add prob.Prob.distinct_consts c;
  BatDynArray.add prob.Prob.distinct_consts d;
  BatDynArray.add prob.Prob.distinct_consts e;

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 1; 1 |];
  Hashtbl.add symb_sorts c [| 0 |];
  Hashtbl.add symb_sorts q [| |];
  Hashtbl.add symb_sorts d [| 0 |];
  Hashtbl.add symb_sorts e [| 0 |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 3; 5 |];
    consts =
      [|
        [| c; d; e |];
        [| |];
      |];
    only_consts = ref true;
  } in

  let sr = Symred.create prob sorts in
  assert_equal [(c, [| |]), (0, 0)] (Symred.incr_max_size sr);
  assert_equal [(d, [| |]), (1, 1)] (Symred.incr_max_size sr);
  assert_equal [(e, [| |]), (2, 2)] (Symred.incr_max_size sr);
  for i = 4 to 100 do
    assert_equal [] (Symred.incr_max_size sr)
  done

let test_distinct_consts_consts () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 2 in
  let c1 = Symb.add_func db 0 in
  let q = Symb.add_pred db 0 in
  let c2 = Symb.add_func db 0 in
  let c3 = Symb.add_func db 0 in
  let d1 = Symb.add_func db 0 in
  let d2 = Symb.add_func db 0 in
  BatDynArray.add prob.Prob.distinct_consts c2;
  BatDynArray.add prob.Prob.distinct_consts c3;

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 0; 2 |];
  Hashtbl.add symb_sorts c1 [| 2 |];
  Hashtbl.add symb_sorts q [| |];
  Hashtbl.add symb_sorts c2 [| 2 |];
  Hashtbl.add symb_sorts c3 [| 2 |];
  Hashtbl.add symb_sorts d1 [| 1 |];
  Hashtbl.add symb_sorts d2 [| 1 |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 1; 0; 4 |];
    consts =
      [|
        [| |];
        [| d1; d2 |];
        [| c1; c2; c3 |];
      |];
    only_consts = ref true;
  } in

  let sr = Symred.create prob sorts in
  assert_equal
    [(c2, [| |]), (0, 0); (d1, [| |]), (0, 0)]
    (Symred.incr_max_size sr);
  assert_equal
    [(c3, [| |]), (1, 1); (d2, [| |]), (0, 1)]
    (Symred.incr_max_size sr);
  assert_equal [(c1, [| |]), (0, 2)] (Symred.incr_max_size sr);
  for i = 4 to 100 do
    assert_equal [] (Symred.incr_max_size sr)
  done

let test_noncomm_func_one_sort () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 2 in
  let f = Symb.add_func db 3 in
  let q = Symb.add_pred db 1 in

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 0; 0 |];
  Hashtbl.add symb_sorts f [| 0; 0; 0; 0 |];
  Hashtbl.add symb_sorts q [| 0 |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 0 |];
    consts =
      [|
        [| |];
      |];
    only_consts = ref false;
  } in

  let sr = Symred.create prob sorts in
  assert_equal [] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 0; 0 |]), (0, 1)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 0; 0 |]), (0, 2)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 0; 1 |]), (0, 3)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 1; 0 |]), (0, 4)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 1; 1 |]), (0, 5)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 1; 0 |]), (0, 6)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 1; 1 |]), (0, 7)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 0; 1 |]), (0, 8)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 2; 0; 0 |]), (0, 9)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 2; 0; 1 |]), (0, 10)] (Symred.incr_max_size sr)

let test_noncomm_func_many_sorts_bounded_res_sort () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 2 in
  let f = Symb.add_func db 4 in
  let q = Symb.add_pred db 1 in

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 0; 3 |];
  Hashtbl.add symb_sorts f [| 2; 1; 0; 3; 2 |];
  Hashtbl.add symb_sorts q [| 1 |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 5; 0; 3; 1 |];
    consts =
      [|
        [| |];
        [| |];
        [| |];
        [| |];
      |];
    only_consts = ref false;
  } in

  let sr = Symred.create prob sorts in
  assert_equal [] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 0; 0; 0 |]), (0, 1)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 0; 0; 0 |]), (0, 2)] (Symred.incr_max_size sr);
  for i = 4 to 100 do
    assert_equal [] (Symred.incr_max_size sr)
  done

let test_noncomm_func_many_sorts_bounded_param_sort () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 2 in
  let f = Symb.add_func db 2 in
  let q = Symb.add_pred db 1 in

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 0; 1 |];
  Hashtbl.add symb_sorts f [| 1; 1; 0 |];
  Hashtbl.add symb_sorts q [| 0 |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 0; 2 |];
    consts =
      [|
        [| |];
        [| |];
      |];
    only_consts = ref false;
  } in

  let sr = Symred.create prob sorts in
  assert_equal [(f, [| 0; 0 |]), (0, 0)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 0 |]), (0, 1)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 1 |]), (0, 2)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 1 |]), (0, 3)] (Symred.incr_max_size sr);
  for i = 5 to 100 do
    assert_equal [] (Symred.incr_max_size sr)
  done

let test_noncomm_func_many_sorts_bounded_param_sorts1 () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 2 in
  let f = Symb.add_func db 2 in
  let q = Symb.add_pred db 1 in

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 0; 1 |];
  Hashtbl.add symb_sorts f [| 2; 1; 0 |];
  Hashtbl.add symb_sorts q [| 0 |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 0; 2; 1 |];
    consts =
      [|
        [| |];
        [| |];
        [| |];
      |];
    only_consts = ref false;
  } in

  let sr = Symred.create prob sorts in
  assert_equal [(f, [| 0; 0 |]), (0, 0)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 1 |]), (0, 1)] (Symred.incr_max_size sr);
  for i = 3 to 100 do
    assert_equal [] (Symred.incr_max_size sr)
  done

let test_noncomm_func_many_sorts_bounded_param_sorts2 () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 3 in
  let f = Symb.add_func db 2 in
  let q = Symb.add_pred db 1 in

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 0; 1; 1 |];
  Hashtbl.add symb_sorts f [| 0; 1; 0 |];
  Hashtbl.add symb_sorts q [| 0 |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 4; 3 |];
    consts =
      [|
        [| |];
        [| |];
      |];
    only_consts = ref false;
  } in

  let sr = Symred.create prob sorts in
  assert_equal [] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 0 |]), (0, 1)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 0 |]), (0, 2)] (Symred.incr_max_size sr);
  (* The cell f(2, 0) is assigned instead of f(1, 1) since the element 1
     is not used in the sort of the second parameter.
  *)
  assert_equal [(f, [| 2; 0 |]), (0, 3)] (Symred.incr_max_size sr);
  for i = 5 to 100 do
    assert_equal [] (Symred.incr_max_size sr)
  done

let test_noncomm_funcs_many_sorts () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 3 in
  let f = Symb.add_func db 2 in
  let q = Symb.add_pred db 0 in
  let g = Symb.add_func db 1 in

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 0; 1; 1 |];
  Hashtbl.add symb_sorts f [| 2; 0; 1 |];
  Hashtbl.add symb_sorts q [| |];
  Hashtbl.add symb_sorts g [| 1; 2 |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 4; 0; 2 |];
    consts =
      [|
        [| |];
        [| |];
        [| |];
      |];
    only_consts = ref false;
  } in

  let sr = Symred.create prob sorts in
  assert_equal [(g, [| 0 |]), (0, 0)] (Symred.incr_max_size sr);
  assert_equal
    [(f, [| 0; 0 |]), (0, 1); (g, [| 1 |]), (0, 1)]
    (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 0 |]), (0, 2)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 1 |]), (0, 3)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 1 |]), (0, 4)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 2 |]), (0, 5)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 2 |]), (0, 6)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 3 |]), (0, 7)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 3 |]), (0, 8)] (Symred.incr_max_size sr);
  for i = 10 to 100 do
    assert_equal [] (Symred.incr_max_size sr)
  done

let test_comm_func_one_sort () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 3 in
  let f = Symb.add_func db 2 in
  let q = Symb.add_pred db 0 in
  Symb.set_commutative db f true;

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 0; 0; 0 |];
  Hashtbl.add symb_sorts f [| 0; 0; 0 |];
  Hashtbl.add symb_sorts q [| |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 8 |];
    consts =
      [|
        [| |];
      |];
    only_consts = ref false;
  } in

  let sr = Symred.create prob sorts in
  assert_equal [] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 0 |]), (0, 1)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 1 |]), (0, 2)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 1 |]), (0, 3)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 2 |]), (0, 4)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 2 |]), (0, 5)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 2; 2 |]), (0, 6)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 3 |]), (0, 7)] (Symred.incr_max_size sr);
  for i = 9 to 100 do
    assert_equal [] (Symred.incr_max_size sr)
  done

let test_comm_funcs_many_sorts () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 1 in
  let f = Symb.add_func db 2 in
  let g = Symb.add_func db 2 in
  let q = Symb.add_pred db 1 in
  Symb.set_commutative db f true;
  Symb.set_commutative db g true;

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 1 |];
  Hashtbl.add symb_sorts f [| 0; 0; 2 |];
  Hashtbl.add symb_sorts g [| 1; 1; 0 |];
  Hashtbl.add symb_sorts q [| 2 |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 4; 0; 6 |];
    consts =
      [|
        [| |];
        [| |];
        [| |];
      |];
    only_consts = ref false;
  } in

  let sr = Symred.create prob sorts in
  assert_equal [(f, [| 0; 0 |]), (0, 0)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 0; 1 |]), (0, 1)] (Symred.incr_max_size sr);
  assert_equal
    [(f, [| 1; 1 |]), (0, 2); (g, [| 0; 0 |]), (0, 2)]
    (Symred.incr_max_size sr);
  assert_equal
    [(f, [| 0; 2 |]), (0, 3); (g, [| 0; 1 |]), (0, 3)]
    (Symred.incr_max_size sr);
  assert_equal [(f, [| 1; 2 |]), (0, 4)] (Symred.incr_max_size sr);
  assert_equal [(f, [| 2; 2 |]), (0, 5)] (Symred.incr_max_size sr);
  for i = 7 to 100 do
    assert_equal [] (Symred.incr_max_size sr);
  done

let test_distinct_consts_const_func () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 2 in
  let f = Symb.add_func db 2 in
  let c1 = Symb.add_func db 0 in
  let c2 = Symb.add_func db 0 in
  let c3 = Symb.add_func db 0 in
  let d = Symb.add_func db 0 in
  BatDynArray.add prob.Prob.distinct_consts c3;
  BatDynArray.add prob.Prob.distinct_consts c1;

  let symb_sorts = Hashtbl.create 20 in
  Hashtbl.add symb_sorts p [| 1; 0 |];
  Hashtbl.add symb_sorts f [| 2; 0; 2 |];
  Hashtbl.add symb_sorts c1 [| 1 |];
  Hashtbl.add symb_sorts c2 [| 1 |];
  Hashtbl.add symb_sorts c3 [| 1 |];
  Hashtbl.add symb_sorts d [| 2 |];
  let sorts = {
    symb_sorts;
    var_sorts = Hashtbl.create 20;
    adeq_sizes = [| 3; 0; 0 |];
    consts =
      [|
        [| |];
        [| c1; c2; c3 |];
        [| d |];
      |];
    only_consts = ref false;
  } in

  let sr = Symred.create prob sorts in
  assert_equal
    [(c3, [| |]), (0, 0); (d, [| |]), (0, 0)]
    (Symred.incr_max_size sr);
  assert_equal
    [(c1, [| |]), (1, 1); (f, [| 0; 0 |]), (0, 1)]
    (Symred.incr_max_size sr);
  assert_equal
    [(c2, [| |]), (0, 2); (f, [| 1; 0 |]), (0, 2)]
    (Symred.incr_max_size sr);
  (* The cell f(2, 0) is assigned instead of the cell f(1, 1) since
     the element 1 is not used in the sort of the second parameter.
  *)
  assert_equal [(f, [| 2; 0 |]), (0, 3)] (Symred.incr_max_size sr);
  for i = 5 to 100 do
    assert_equal [(f, [| i-2; 0 |]), (0, i-1)] (Symred.incr_max_size sr)
  done

let suite =
  "Symred suite" >:::
    [
      "no consts, no funcs" >:: test_no_consts_no_funcs;
      "consts in same sort" >:: test_consts_in_same_sort;
      "consts in different sorts" >:: test_consts_in_diff_sorts;
      "distinct consts" >:: test_distinct_consts;
      "distinct consts, consts" >:: test_distinct_consts_consts;
      "noncommutative func - one sort" >:: test_noncomm_func_one_sort;
      "noncommutative func - many sorts, bounded result sort" >::
        test_noncomm_func_many_sorts_bounded_res_sort;
      "noncommutative func - many sorts, bounded param sort" >::
        test_noncomm_func_many_sorts_bounded_param_sort;
      "noncommutative func - many sorts, bounded param sorts 1" >::
        test_noncomm_func_many_sorts_bounded_param_sorts1;
      "noncommutative func - many sorts, bounded param sorts 2" >::
        test_noncomm_func_many_sorts_bounded_param_sorts2;
      "noncommutative funcs - many sorts" >:: test_noncomm_funcs_many_sorts;
      "commutative func - one sort" >:: test_comm_func_one_sort;
      "commutative funcs - many sorts" >:: test_comm_funcs_many_sorts;
      "distinct consts, const, func" >:: test_distinct_consts_const_func;
    ]
