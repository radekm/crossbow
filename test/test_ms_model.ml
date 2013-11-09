(* Copyright (c) 2013 Radek Micek *)

open OUnit

module T = Term
module L = Lit
module C = Clause2
module Ms = Ms_model

let map_of_list xs = Symb.Map.of_enum (BatList.enum xs)

let test_canonize_one_sort_size_one () =
  let prob = Prob.create () in
  let db = prob.Prob.symbols in
  let c = Symb.add_func db 0 in
  let clause =
    let x = T.var 0 in
    {
      C.cl_id = Prob.fresh_id prob;
      (* c = x *)
      C.cl_lits = [ L.mk_eq (T.func (c, [| |])) x ];
    } in
  BatDynArray.add prob.Prob.clauses clause;
  let sorts = Sorts.of_problem prob in

  let model = {
    Ms.max_size = 1;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
      ];
  } in

  assert_equal ~cmp:Ms.equal model (Ms.canonize model sorts)

let test_canonize_one_sort_size_two () =
  let prob = Prob.create () in
  let db = prob.Prob.symbols in
  let c = Symb.add_func db 0 in
  let p = Symb.add_pred db 1 in
  let f = Symb.add_func db 1 in
  let r = Symb.add_pred db 0 in
  let clause =
    let x = T.var 0 in
    {
      C.cl_id = Prob.fresh_id prob;
      (* c = x, ~p(x), f(x) = x, r *)
      C.cl_lits = [
        L.mk_eq (T.func (c, [| |])) x;
        L.lit (Sh.Neg, p, [| x |]);
        L.mk_eq (T.func (f, [| x |])) x;
        L.lit (Sh.Pos, r, [| |]);
      ];
    } in
  BatDynArray.add prob.Prob.clauses clause;
  let sorts = Sorts.of_problem prob in

  let model = {
    Ms.max_size = 2;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 1 |];
        };
        p, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 0; 1 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 1; 0 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 1 |];
        };
      ];
  } in

  let model2 = {
    Ms.max_size = 2;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
        p, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 1; 0 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 1; 0 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 1 |];
        };
      ];
  } in

  let m = Ms.canonize model sorts in
  assert_bool "" (List.exists (Ms.equal m) [model; model2]);
  assert_equal ~cmp:Ms.equal m (Ms.canonize model2 sorts)

let test_canonize_two_sorts_sizes_two_three () =
  let prob = Prob.create () in
  let db = prob.Prob.symbols in
  let c = Symb.add_func db 0 in
  let p = Symb.add_pred db 2 in
  let f = Symb.add_func db 1 in
  let r = Symb.add_pred db 0 in
  let clause =
    let x = T.var 0 in
    let y = T.var 1 in
    {
      C.cl_id = Prob.fresh_id prob;
      (* c = x, ~p(x, y), f(x) <> y, ~r *)
      C.cl_lits = [
        L.mk_eq (T.func (c, [| |])) x;
        L.lit (Sh.Neg, p, [| x; y |]);
        L.mk_ineq (T.func (f, [| x |])) y;
        L.lit (Sh.Neg, r, [| |]);
      ];
    } in
  BatDynArray.add prob.Prob.clauses clause;
  let sorts = Sorts.of_problem prob in

  let model = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 1 |];
        };
        p, {
          Ms.param_sizes = [| 2; 3 |];
          Ms.values = [| 1; 0; 1; 0; 0; 0 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 1; 2 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
      ];
  } in

  (* permutation: 0 1; permutation: 0 2 1 *)
  let model2 = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 1 |];
        };
        p, {
          Ms.param_sizes = [| 2; 3 |];
          Ms.values = [| 1; 1; 0; 0; 0; 0 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 2; 1 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
      ];
  } in

  (* permutation: 0 1; permutation: 1 0 2 *)
  let model3 = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 1 |];
        };
        p, {
          Ms.param_sizes = [| 2; 3 |];
          Ms.values = [| 0; 1; 1; 0; 0; 0 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 0; 2 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
      ];
  } in

  (* permutation: 0 1; permutation: 1 2 0 *)
  let model4 = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 1 |];
        };
        p, {
          Ms.param_sizes = [| 2; 3 |];
          Ms.values = [| 1; 1; 0; 0; 0; 0 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 2; 0 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
      ];
  } in

  (* permutation: 0 1; permutation: 2 0 1 *)
  let model5 = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 1 |];
        };
        p, {
          Ms.param_sizes = [| 2; 3 |];
          Ms.values = [| 0; 1; 1; 0; 0; 0 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 0; 1 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
      ];
  } in

  (* permutation: 0 1; permutation: 2 1 0 *)
  let model6 = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 1 |];
        };
        p, {
          Ms.param_sizes = [| 2; 3 |];
          Ms.values = [| 1; 0; 1; 0; 0; 0 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 1; 0 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
      ];
  } in

  (* permutation: 1 0; permutation: 0 1 2 *)
  let model7 = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
        p, {
          Ms.param_sizes = [| 2; 3 |];
          Ms.values = [| 0; 0; 0; 1; 0; 1 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 2; 1 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
      ];
  } in

  (* permutation: 1 0; permutation: 0 2 1 *)
  let model8 = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
        p, {
          Ms.param_sizes = [| 2; 3 |];
          Ms.values = [| 0; 0; 0; 1; 1; 0 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 1; 2 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
      ];
  } in

  (* permutation: 1 0; permutation: 1 0 2 *)
  let model9 = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
        p, {
          Ms.param_sizes = [| 2; 3 |];
          Ms.values = [| 0; 0; 0; 0; 1; 1 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 2; 0 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
      ];
  } in

  (* permutation: 1 0; permutation: 1 2 0 *)
  let model10 = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
        p, {
          Ms.param_sizes = [| 2; 3 |];
          Ms.values = [| 0; 0; 0; 1; 1; 0 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 0; 2 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
      ];
  } in

  (* permutation: 1 0; permutation: 2 0 1 *)
  let model11 = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
        p, {
          Ms.param_sizes = [| 2; 3 |];
          Ms.values = [| 0; 0; 0; 0; 1; 1 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 1; 0 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
      ];
  } in

  (* permutation: 1 0; permutation: 2 1 0 *)
  let model12 = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list [
        c, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
        p, {
          Ms.param_sizes = [| 2; 3 |];
          Ms.values = [| 0; 0; 0; 1; 0; 1 |];
        };
        f, {
          Ms.param_sizes = [| 2 |];
          Ms.values = [| 0; 1 |];
        };
        r, {
          Ms.param_sizes = [| |];
          Ms.values = [| 0 |];
        };
      ];
  } in

  let all_models = [
    model; model2; model3; model4; model5; model6;
    model7; model8; model9; model10; model11; model12;
  ] in

  let m = Ms.canonize model sorts in
  assert_bool "" (List.exists (Ms.equal m) all_models);
  List.iter
    (fun m' -> assert_equal ~cmp:Ms.equal m (Ms.canonize m' sorts))
    all_models

let suite =
  "Ms_model suite" >:::
    [
      "canonize - one sort, size one" >:: test_canonize_one_sort_size_one;
      "canonize - one sort, size two" >:: test_canonize_one_sort_size_two;
      "canonize - two sorts, sizes two, three" >::
        test_canonize_two_sorts_sizes_two_three;
    ]
