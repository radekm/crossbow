(* Copyright (c) 2013 Radek Micek *)

open OUnit

module C = Clause
module T = Term

let test_lnh_one_sort () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let f = Symb.add db 2 in
  let x = T.Var 0 in
  let cl = {
    C.cl_id = 1;
    C.cl_lits = [ T.mk_eq (T.Func (f, [| x; x; |])) x ];
  } in
  BatDynArray.add prob.Prob.clauses cl;
  let sorts = Sorts.of_problem prob in

  let proc_cells = [
    (f, [| 0; 0 |]), (0, 0);
  ] in
  let cell = (f, [| 0; 1 |]), (0, 2) in
  let exp_constrs = [| |] in
  let constrs = Lnh.lnh db sorts proc_cells cell in
  assert_equal exp_constrs constrs;

  let cell2 = (f, [| 0; 1 |]), (0, 3) in
  let exp_constrs2 =
    [|
      {
        Lnh.assig = (f, [| 0; 1 |]), 3;
        Lnh.required = [| |], 2;
      };
    |] in
  let constrs2 = Lnh.lnh db sorts proc_cells cell2 in
  assert_equal exp_constrs2 constrs2

let test_lnh_more_sorts () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let f = Symb.add db 2 in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let cl = {
    C.cl_id = 1;
    C.cl_lits = [ T.mk_eq (T.Func (f, [| x; y |])) y ];
  } in
  BatDynArray.add prob.Prob.clauses cl;
  let sorts = Sorts.of_problem prob in

  let proc_cells = [
    (f, [| 1; 0 |]), (1, 1);
    (f, [| 2; 0 |]), (2, 3);
  ] in
  let cell = (f, [| 0; 0 |]), (0, 5) in
  let exp_constrs =
    [|
      {
        Lnh.assig = (f, [| 0; 0 |]), 3;
        Lnh.required = [| f, [| 2; 0 |] |], 2;
      };
      {
        Lnh.assig = (f, [| 0; 0 |]), 4;
        Lnh.required = [| f, [| 2; 0 |] |], 3;
      };
      {
        Lnh.assig = (f, [| 0; 0 |]), 5;
        Lnh.required = [| |], 4;
      };
    |] in
  let constrs = Lnh.lnh db sorts proc_cells cell in
  assert_equal exp_constrs constrs;

  let proc_cells2 = [
    (f, [| 0; 2 |]), (0, 1);
  ] in
  let cell2 = (f, [| 0; 0 |]), (0, 2) in
  let exp_constrs2 = [| |] in
  let constrs2 = Lnh.lnh db sorts proc_cells2 cell2 in
  assert_equal exp_constrs2 constrs2;

  let proc_cells3 = [
    (f, [| 0; 0 |]), (1, 2);
    (f, [| 1; 0 |]), (1, 2);
  ] in
  let cell3 = (f, [| 2; 0 |]), (0, 3) in
  let exp_constrs3 =
    [|
      {
        Lnh.assig = (f, [| 2; 0 |]), 2;
        Lnh.required = [| f, [| 0; 0 |]; f, [| 1; 0 |] |], 1;
      };
      {
        Lnh.assig = (f, [| 2; 0 |]), 3;
        Lnh.required = [| f, [| 0; 0 |]; f, [| 1; 0 |] |], 2;
      };
    |] in
  let constrs3 = Lnh.lnh db sorts proc_cells3 cell3 in
  assert_equal exp_constrs3 constrs3

let suite =
  "Lnh suite" >:::
    [
      "lnh - one sort" >:: test_lnh_one_sort;
      "lnh - more sorts" >:: test_lnh_more_sorts;
    ]
