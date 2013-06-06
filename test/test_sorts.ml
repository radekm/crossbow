(* Copyright (c) 2013 Radek Micek *)

open OUnit

module S = Symb
module T = Term
module C = Clause


(* Asserts that the given arrays are equal up to the order of the elements. *)
let assert_same_elems arr1 arr2 =
  let arr1 = Array.copy arr1 in
  let arr2 = Array.copy arr2 in
  Array.sort compare arr1;
  Array.sort compare arr2;
  assert_equal arr1 arr2


let test_one_sort_adeq_size () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let c1 = S.add_func db 0 in
  let c2 = S.add_func db 0 in
  let c3 = S.add_func db 0 in
  let x = T.Var 5 in
  (* x = c1, x = c2, x = c3 *)
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    C.cl_lits = [
      T.mk_eq x (T.Func (c1, [| |]));
      T.mk_eq x (T.Func (c2, [| |]));
      T.mk_eq x (T.Func (c3, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;
  let sorts = Sorts.of_problem prob in

  let symb_sorts = sorts.Sorts.symb_sorts in
  let var_sorts = sorts.Sorts.var_sorts in
  assert_equal 3 (Hashtbl.length symb_sorts);
  assert_equal 1 (Hashtbl.length var_sorts);

  assert_equal 1 (Array.length (Hashtbl.find symb_sorts c1));
  assert_equal 1 (Array.length (Hashtbl.find symb_sorts c2));
  assert_equal 1 (Array.length (Hashtbl.find symb_sorts c3));

  let c1_sort = (Hashtbl.find symb_sorts c1).(0) in
  let c2_sort = (Hashtbl.find symb_sorts c2).(0) in
  let c3_sort = (Hashtbl.find symb_sorts c3).(0) in
  let x_sort = Hashtbl.find var_sorts (clause.C.cl_id, 5) in
  Eunit.assert_partition [[c1_sort; c2_sort; c3_sort; x_sort]];

  let adeq_sizes = sorts.Sorts.adeq_sizes in
  assert_equal 1 (Array.length adeq_sizes);
  assert_equal 4 adeq_sizes.(c1_sort);

  let consts = sorts.Sorts.consts in
  assert_equal 1 (Array.length consts);
  assert_same_elems [| c1; c2; c3 |] consts.(c1_sort);

  assert_bool "" !(sorts.Sorts.only_consts)


let test_one_sort_no_adeq_size () =
  let Prob.Wr prob = Prob.create () in
  let x = T.Var 5 in
  let y = T.Var 2 in
  let z = T.Var 3 in
  (* x = y, y = z, x = z *)
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    C.cl_lits = [
      T.mk_eq x y;
      T.mk_eq y z;
      T.mk_eq x z;
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;
  let sorts = Sorts.of_problem prob in

  let symb_sorts = sorts.Sorts.symb_sorts in
  let var_sorts = sorts.Sorts.var_sorts in
  assert_equal 0 (Hashtbl.length symb_sorts);
  assert_equal 3 (Hashtbl.length var_sorts);

  let x_sort = Hashtbl.find var_sorts (clause.C.cl_id, 5) in
  let y_sort = Hashtbl.find var_sorts (clause.C.cl_id, 2) in
  let z_sort = Hashtbl.find var_sorts (clause.C.cl_id, 3) in
  Eunit.assert_partition [[x_sort; y_sort; z_sort]];

  let adeq_sizes = sorts.Sorts.adeq_sizes in
  assert_equal 1 (Array.length adeq_sizes);
  assert_equal 0 adeq_sizes.(x_sort);

  let consts = sorts.Sorts.consts in
  assert_equal 1 (Array.length consts);
  assert_same_elems [| |] consts.(x_sort);

  assert_bool "" !(sorts.Sorts.only_consts)


(* One sort contains the equality of variables
   and the other contains function symbol with arity > 0.
*)
let test_no_adeq_size_both_reasons () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let f = S.add_func db 1 in
  let x = T.Var 5431 in
  let y = T.Var 16000 in
  (* x = y, f(x) != f(y) *)
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    C.cl_lits = [
      T.mk_eq x y;
      T.mk_ineq (T.Func (f, [| x |])) (T.Func (f, [| y |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;
  let sorts = Sorts.of_problem prob in

  let symb_sorts = sorts.Sorts.symb_sorts in
  let var_sorts = sorts.Sorts.var_sorts in
  assert_equal 1 (Hashtbl.length symb_sorts);
  assert_equal 2 (Hashtbl.length var_sorts);

  assert_equal 2 (Array.length (Hashtbl.find symb_sorts f));

  let f_par1_sort = (Hashtbl.find symb_sorts f).(0) in
  let f_sort = (Hashtbl.find symb_sorts f).(1) in
  let x_sort = Hashtbl.find var_sorts (clause.C.cl_id, 5431) in
  let y_sort = Hashtbl.find var_sorts (clause.C.cl_id, 16000) in
  Eunit.assert_partition [[x_sort; y_sort; f_par1_sort]; [f_sort]];

  let adeq_sizes = sorts.Sorts.adeq_sizes in
  assert_equal 2 (Array.length adeq_sizes);
  assert_equal 0 adeq_sizes.(x_sort);
  assert_equal 0 adeq_sizes.(f_sort);

  let consts = sorts.Sorts.consts in
  assert_equal 2 (Array.length consts);
  assert_same_elems [| |] consts.(x_sort);
  assert_same_elems [| |] consts.(f_sort);

  assert_bool "" (not !(sorts.Sorts.only_consts))


let test_more_clauses () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let c1 = S.add_func db 0 in
  let c2 = S.add_func db 0 in
  let c3 = S.add_func db 0 in
  let c4 = S.add_func db 0 in
  let x = T.Var 0 in
  (* c1 != c2 *)
  let clause1 = {
    C.cl_id = Prob.fresh_id prob;
    C.cl_lits = [
      T.mk_ineq (T.Func (c1, [| |])) (T.Func (c2, [| |]));
    ];
  } in
  (* x = c1, x = c2, c3 = c4 *)
  let clause2 = {
    C.cl_id = Prob.fresh_id prob;
    C.cl_lits = [
      T.mk_eq x (T.Func (c1, [| |]));
      T.mk_eq x (T.Func (c2, [| |]));
      T.mk_eq (T.Func (c3, [| |])) (T.Func (c4, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause1;
  BatDynArray.add prob.Prob.clauses clause2;
  let sorts = Sorts.of_problem prob in

  let symb_sorts = sorts.Sorts.symb_sorts in
  let var_sorts = sorts.Sorts.var_sorts in
  assert_equal 4 (Hashtbl.length symb_sorts);
  assert_equal 1 (Hashtbl.length var_sorts);

  assert_equal 1 (Array.length (Hashtbl.find symb_sorts c1));
  assert_equal 1 (Array.length (Hashtbl.find symb_sorts c2));
  assert_equal 1 (Array.length (Hashtbl.find symb_sorts c3));
  assert_equal 1 (Array.length (Hashtbl.find symb_sorts c4));

  let c1_sort = (Hashtbl.find symb_sorts c1).(0) in
  let c2_sort = (Hashtbl.find symb_sorts c2).(0) in
  let c3_sort = (Hashtbl.find symb_sorts c3).(0) in
  let c4_sort = (Hashtbl.find symb_sorts c4).(0) in
  let x_sort = Hashtbl.find var_sorts (clause2.C.cl_id, 0) in
  Eunit.assert_partition [[c1_sort; c2_sort; x_sort]; [c3_sort; c4_sort]];

  let adeq_sizes = sorts.Sorts.adeq_sizes in
  assert_equal 2 (Array.length adeq_sizes);
  assert_equal 3 adeq_sizes.(c1_sort);
  assert_equal 2 adeq_sizes.(c3_sort);

  let consts = sorts.Sorts.consts in
  assert_equal 2 (Array.length consts);
  assert_same_elems [| c1; c2 |] consts.(c1_sort);
  assert_same_elems [| c3; c4 |] consts.(c3_sort);

  assert_bool "" !(sorts.Sorts.only_consts)


let test_predicate () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let le = S.add_pred db 2 in
  let succ = S.add_func db 1 in
  let zero = S.add_func db 0 in
  let x = T.Var 0 in
  let two = T.Func (succ, [| T.Func (succ, [| T.Func (zero, [| |]) |]) |]) in
  (* ~le(succ(x), succ(succ(zero))), x = zero, succ(zero) = x *)
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    C.cl_lits = [
      C.neg_lit (T.Func (le, [| T.Func (succ, [| x |]); two |]));
      T.mk_eq x (T.Func (zero, [| |]));
      T.mk_eq (T.Func (succ, [| T.Func (zero, [| |]) |])) x;
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;
  let sorts = Sorts.of_problem prob in

  let symb_sorts = sorts.Sorts.symb_sorts in
  let var_sorts = sorts.Sorts.var_sorts in
  assert_equal 3 (Hashtbl.length symb_sorts);
  assert_equal 1 (Hashtbl.length var_sorts);

  assert_equal 2 (Array.length (Hashtbl.find symb_sorts le));
  assert_equal 2 (Array.length (Hashtbl.find symb_sorts succ));
  assert_equal 1 (Array.length (Hashtbl.find symb_sorts zero));

  let le_par1_sort = (Hashtbl.find symb_sorts le).(0) in
  let le_par2_sort = (Hashtbl.find symb_sorts le).(1) in
  let succ_par1_sort = (Hashtbl.find symb_sorts succ).(0) in
  let succ_sort = (Hashtbl.find symb_sorts succ).(1) in
  let zero_sort = (Hashtbl.find symb_sorts zero).(0) in
  let x_sort = Hashtbl.find var_sorts (clause.C.cl_id, 0) in
  Eunit.assert_partition [
    [le_par1_sort; le_par2_sort; succ_par1_sort; succ_sort; zero_sort; x_sort];
  ];

  let adeq_sizes = sorts.Sorts.adeq_sizes in
  assert_equal 1 (Array.length adeq_sizes);
  assert_equal 0 adeq_sizes.(x_sort);

  let consts = sorts.Sorts.consts in
  assert_equal 1 (Array.length consts);
  assert_same_elems [| zero |] consts.(x_sort);

  assert_bool "" (not !(sorts.Sorts.only_consts))


let test_func_high_arity () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let f = S.add_func db 4 in
  let g = S.add_func db 1 in
  let c = S.add_func db 0 in
  let x = T.Var 7 in
  let y = T.Var 8 in
  let z = T.Var 5 in
  (* f(x, y, g(z), c) = y *)
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    C.cl_lits = [
      T.mk_eq (T.Func (f, [| x; y; T.Func (g, [| z |]); T.Func (c, [| |]) |])) y;
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;
  let sorts = Sorts.of_problem prob in

  let symb_sorts = sorts.Sorts.symb_sorts in
  let var_sorts = sorts.Sorts.var_sorts in
  assert_equal 3 (Hashtbl.length symb_sorts);
  assert_equal 3 (Hashtbl.length var_sorts);

  assert_equal 5 (Array.length (Hashtbl.find symb_sorts f));
  assert_equal 2 (Array.length (Hashtbl.find symb_sorts g));
  assert_equal 1 (Array.length (Hashtbl.find symb_sorts c));

  let f_par1_sort = (Hashtbl.find symb_sorts f).(0) in
  let f_par2_sort = (Hashtbl.find symb_sorts f).(1) in
  let f_par3_sort = (Hashtbl.find symb_sorts f).(2) in
  let f_par4_sort = (Hashtbl.find symb_sorts f).(3) in
  let f_sort = (Hashtbl.find symb_sorts f).(4) in
  let g_par1_sort = (Hashtbl.find symb_sorts g).(0) in
  let g_sort = (Hashtbl.find symb_sorts g).(1) in
  let c_sort = (Hashtbl.find symb_sorts c).(0) in
  let x_sort = Hashtbl.find var_sorts (clause.C.cl_id, 7) in
  let y_sort = Hashtbl.find var_sorts (clause.C.cl_id, 8) in
  let z_sort = Hashtbl.find var_sorts (clause.C.cl_id, 5) in
  Eunit.assert_partition [
    [f_par1_sort; x_sort];
    [f_par2_sort; f_sort; y_sort];
    [f_par3_sort; g_sort];
    [f_par4_sort; c_sort];
    [g_par1_sort; z_sort];
  ];

  let adeq_sizes = sorts.Sorts.adeq_sizes in
  assert_equal 5 (Array.length adeq_sizes);
  assert_equal 1 adeq_sizes.(f_par1_sort);
  assert_equal 0 adeq_sizes.(f_par2_sort);
  assert_equal 0 adeq_sizes.(f_par3_sort);
  assert_equal 1 adeq_sizes.(f_par4_sort);
  assert_equal 1 adeq_sizes.(g_par1_sort);

  let consts = sorts.Sorts.consts in
  assert_equal 5 (Array.length consts);
  assert_same_elems [| |] consts.(f_par1_sort);
  assert_same_elems [| |] consts.(f_par2_sort);
  assert_same_elems [| |] consts.(f_par3_sort);
  assert_same_elems [| c |] consts.(f_par4_sort);
  assert_same_elems [| |] consts.(g_par1_sort);

  assert_bool "" (not !(sorts.Sorts.only_consts))


let suite =
  "Sorts suite" >:::
    [
      "one sort - adequate size" >:: test_one_sort_adeq_size;
      "one sort - no adequate size" >:: test_one_sort_no_adeq_size;
      "no adequate size - both reasons" >:: test_no_adeq_size_both_reasons;
      "more clauses" >:: test_more_clauses;
      "predicate" >:: test_predicate;
      "function with high arity" >:: test_func_high_arity;
    ]
