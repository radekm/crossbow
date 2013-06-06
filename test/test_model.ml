(* Copyright (c) 2013 Radek Micek *)

open OUnit

module C = Clause
module T = Term
module Ms = Ms_model
module M = Model

let hashtbl_of_list xs = BatHashtbl.of_enum (BatList.enum xs)

let hashtbl_to_list h =
  BatList.sort (BatList.of_enum (BatHashtbl.enum h))

let model_eq m1 m2 =
  m1.M.max_size = m2.M.max_size &&
  hashtbl_to_list m1.M.symbs = hashtbl_to_list m2.M.symbs

let show_model m =
  let b = Buffer.create 128 in
  Printf.bprintf b "Max size: %d\n" m.M.max_size;
  List.iter
    (fun (symb, table) ->
      Printf.bprintf b "Symbol %2d: [" (Symb.id_to_int symb);
      Array.iter (Printf.bprintf b " %2d") table.M.values;
      Printf.bprintf b " ]\n")
    (hashtbl_to_list m.M.symbs);
  Buffer.contents b

let build_and_check_model prob ms_model exp_model =
  let sorts = Sorts.of_problem prob in
  let model = M.of_ms_model ms_model sorts in
  assert_equal ~cmp:model_eq ~printer:show_model exp_model model

let test_nullary_pred () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add db 0 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* p *)
    C.cl_lits = [ T.Func (p, [| |]) ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model = {
    Ms.max_size = 1;
    Ms.symbs =
      hashtbl_of_list
        [
          p, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
        ];
  } in
  let exp_model = {
    M.max_size = 1;
    M.symbs =
      hashtbl_of_list
        [
          p, { M.values = [| 1 |] };
        ];
  } in
  build_and_check_model prob ms_model exp_model

let test_unary_func_one_sort_no_adeq_size () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let f = Symb.add db 1 in
  let x = T.Var 0 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x) <> x *)
    C.cl_lits = [ T.mk_ineq (T.Func (f, [| x |])) x ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model = {
    Ms.max_size = 3;
    Ms.symbs =
      hashtbl_of_list
        [
          f, { Ms.param_sizes = [| 3 |]; Ms.values = [| 1; 2; 1 |] };
        ];
  } in
  let exp_model = {
    M.max_size = 3;
    M.symbs =
      hashtbl_of_list
        [
          f, { M.values = [| 1; 2; 1 |] };
        ];
  } in
  build_and_check_model prob ms_model exp_model

let test_binary_pred_two_sorts_no_adeq_size () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add db 2 in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let u = T.Var 2 in
  let v = T.Var 3 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* p(x, y), x = u, y = v *)
    C.cl_lits = [
      T.Func (p, [| x; y; |]);
      T.mk_eq x u;
      T.mk_eq y v;
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model = {
    Ms.max_size = 2;
    Ms.symbs =
      hashtbl_of_list
        [
          p, { Ms.param_sizes = [| 2; 2 |]; Ms.values = [| 1; 1; 1; 1 |] };
        ];
  } in
  let exp_model = {
    M.max_size = 2;
    M.symbs =
      hashtbl_of_list
        [
          p, { M.values = [| 1; 1; 1; 1 |] };
        ];
  } in
  build_and_check_model prob ms_model exp_model

let test_unary_pred_one_sort_adeq_size () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add db 1 in
  let c = Symb.add db 0 in
  let d = Symb.add db 0 in
  let x = T.Var 0 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* ~p(x), x = c, x = d *)
    C.cl_lits = [
      C.neg_lit (T.Func (p, [| x |]));
      T.mk_eq x (T.Func (c, [| |]));
      T.mk_eq x (T.Func (d, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model2 = {
    Ms.max_size = 2;
    Ms.symbs =
      hashtbl_of_list
        [
          p, { Ms.param_sizes = [| 2 |]; Ms.values = [| 0; 1 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
        ];
  } in
  let exp_model2 = {
    M.max_size = 2;
    M.symbs =
      hashtbl_of_list
        [
          p, { M.values = [| 0; 1 |] };
          c, { M.values = [| 1 |] };
          d, { M.values = [| 1 |] };
        ];
  } in
  build_and_check_model prob ms_model2 exp_model2;

  let ms_model3 = {
    Ms.max_size = 3;
    Ms.symbs =
      hashtbl_of_list
        [
          p, { Ms.param_sizes = [| 3 |]; Ms.values = [| 0; 1; 1 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 2 |] };
        ];
  } in
  let exp_model3 = {
    M.max_size = 3;
    M.symbs =
      hashtbl_of_list
        [
          p, { M.values = [| 0; 1; 1 |] };
          c, { M.values = [| 1 |] };
          d, { M.values = [| 2 |] };
        ];
  } in
  build_and_check_model prob ms_model3 exp_model3;

  let ms_model4 = {
    Ms.max_size = 4;
    Ms.symbs =
      hashtbl_of_list
        [
          p, { Ms.param_sizes = [| 3 |]; Ms.values = [| 0; 1; 1 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 2 |] };
        ];
  } in
  let exp_model4 = {
    M.max_size = 4;
    M.symbs =
      hashtbl_of_list
        [
          p, { M.values = [| 0; 1; 1; 0 |] };
          c, { M.values = [| 1 |] };
          d, { M.values = [| 2 |] };
        ];
  } in
  build_and_check_model prob ms_model4 exp_model4

(* Same as previous, but contains inequalities instead of equalities. *)
let test_unary_pred_one_sort_adeq_size2 () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add db 1 in
  let c = Symb.add db 0 in
  let d = Symb.add db 0 in
  let x = T.Var 0 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* ~p(x), x <> c, x <> d *)
    C.cl_lits = [
      C.neg_lit (T.Func (p, [| x |]));
      T.mk_ineq x (T.Func (c, [| |]));
      T.mk_ineq x (T.Func (d, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model2 = {
    Ms.max_size = 2;
    Ms.symbs =
      hashtbl_of_list
        [
          p, { Ms.param_sizes = [| 2 |]; Ms.values = [| 1; 0 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
        ];
  } in
  let exp_model2 = {
    M.max_size = 2;
    M.symbs =
      hashtbl_of_list
        [
          p, { M.values = [| 1; 0 |] };
          c, { M.values = [| 1 |] };
          d, { M.values = [| 1 |] };
        ];
  } in
  build_and_check_model prob ms_model2 exp_model2;

  let ms_model3 = {
    Ms.max_size = 3;
    Ms.symbs =
      hashtbl_of_list
        [
          p, { Ms.param_sizes = [| 2 |]; Ms.values = [| 1; 0 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
        ];
  } in
  let exp_model3 = {
    M.max_size = 3;
    M.symbs =
      hashtbl_of_list
        [
          p, { M.values = [| 1; 0; 1 |] };
          c, { M.values = [| 1 |] };
          d, { M.values = [| 1 |] };
        ];
  } in
  build_and_check_model prob ms_model3 exp_model3

let test_binary_func_two_sorts_one_adeq_size () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let f = Symb.add db 2 in
  let c = Symb.add db 0 in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x, y) = y, x = c *)
    C.cl_lits = [
      T.mk_eq (T.Func (f, [| x; y |])) y;
      T.mk_eq x (T.Func (c, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model1 = {
    Ms.max_size = 1;
    Ms.symbs =
      hashtbl_of_list
        [
          f, { Ms.param_sizes = [| 1; 1 |]; Ms.values = [| 0 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
        ];
  } in
  let exp_model1 = {
    M.max_size = 1;
    M.symbs =
      hashtbl_of_list
        [
          f, { M.values = [| 0 |] };
          c, { M.values = [| 0 |] };
        ];
  } in
  build_and_check_model prob ms_model1 exp_model1;

  let ms_model2 = {
    Ms.max_size = 2;
    Ms.symbs =
      hashtbl_of_list
        [
          f, { Ms.param_sizes = [| 2; 2 |]; Ms.values = [| 1; 0; 0; 1 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
        ];
  } in
  let exp_model2 = {
    M.max_size = 2;
    M.symbs =
      hashtbl_of_list
        [
          f, { M.values = [| 1; 0; 0; 1 |] };
          c, { M.values = [| 0 |] };
        ];
  } in
  build_and_check_model prob ms_model2 exp_model2;

  let ms_model3 = {
    Ms.max_size = 3;
    Ms.symbs =
      hashtbl_of_list
        [
          f,
          {
            Ms.param_sizes = [| 2; 3 |];
            Ms.values = [| 1; 0; 0; 0; 1; 2 |]
          };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
        ];
  } in
  let exp_model3 = {
    M.max_size = 3;
    M.symbs =
      hashtbl_of_list
        [
          f, { M.values = [| 1; 0; 0; 0; 1; 2; 0; 1; 2 |] };
          c, { M.values = [| 0 |] };
        ];
  } in
  build_and_check_model prob ms_model3 exp_model3

(* Same as previous, but now the first argument of f has no adequate size
   and the second argument has.
*)
let test_binary_func_two_sorts_one_adeq_size2 () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let f = Symb.add db 2 in
  let c = Symb.add db 0 in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x, y) = x, y = c *)
    C.cl_lits = [
      T.mk_eq (T.Func (f, [| x; y |])) x;
      T.mk_eq y (T.Func (c, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model1 = {
    Ms.max_size = 1;
    Ms.symbs =
      hashtbl_of_list
        [
          f, { Ms.param_sizes = [| 1; 1 |]; Ms.values = [| 0 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
        ];
  } in
  let exp_model1 = {
    M.max_size = 1;
    M.symbs =
      hashtbl_of_list
        [
          f, { M.values = [| 0 |] };
          c, { M.values = [| 0 |] };
        ];
  } in
  build_and_check_model prob ms_model1 exp_model1;

  let ms_model2 = {
    Ms.max_size = 2;
    Ms.symbs =
      hashtbl_of_list
        [
          f, { Ms.param_sizes = [| 2; 2 |]; Ms.values = [| 1; 0; 0; 1 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
        ];
  } in
  let exp_model2 = {
    M.max_size = 2;
    M.symbs =
      hashtbl_of_list
        [
          f, { M.values = [| 1; 0; 0; 1 |] };
          c, { M.values = [| 0 |] };
        ];
  } in
  build_and_check_model prob ms_model2 exp_model2;

  let ms_model3 = {
    Ms.max_size = 3;
    Ms.symbs =
      hashtbl_of_list
        [
          f,
          {
            Ms.param_sizes = [| 3; 2 |];
            Ms.values = [| 2; 0; 2; 1; 1; 2 |]
          };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
        ];
  } in
  let exp_model3 = {
    M.max_size = 3;
    M.symbs =
      hashtbl_of_list
        [
          f, { M.values = [| 2; 0; 0; 2; 1; 1; 1; 2; 2 |] };
          c, { M.values = [| 0 |] };
        ];
  } in
  build_and_check_model prob ms_model3 exp_model3

let test_binary_func_three_sorts_two_adeq_sizes () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let f = Symb.add db 2 in
  let c1 = Symb.add db 0 in
  let c2 = Symb.add db 0 in
  let d1 = Symb.add db 0 in
  let d2 = Symb.add db 0 in
  let e = Symb.add db 0 in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let z = T.Var 2 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x, y) = z, x <> c1, x <> c2, y = d1, y = d2, z <> e *)
    C.cl_lits = [
      T.mk_eq (T.Func (f, [| x; y |])) z;
      T.mk_ineq x (T.Func (c1, [| |]));
      T.mk_ineq x (T.Func (c2, [| |]));
      T.mk_eq y (T.Func (d1, [| |]));
      T.mk_eq y (T.Func (d2, [| |]));
      T.mk_ineq z (T.Func (e, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model1 = {
    Ms.max_size = 1;
    Ms.symbs =
      hashtbl_of_list
        [
          f, { Ms.param_sizes = [| 1; 1 |]; Ms.values = [| 0 |] };
          c1, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
          c2, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
          d1, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
          d2, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
          e, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
        ];
  } in
  let exp_model1 = {
    M.max_size = 1;
    M.symbs =
      hashtbl_of_list
        [
          f, { M.values = [| 0 |] };
          c1, { M.values = [| 0 |] };
          c2, { M.values = [| 0 |] };
          d1, { M.values = [| 0 |] };
          d2, { M.values = [| 0 |] };
          e, { M.values = [| 0 |] };
        ];
  } in
  build_and_check_model prob ms_model1 exp_model1;

  let ms_model2 = {
    Ms.max_size = 2;
    Ms.symbs =
      hashtbl_of_list
        [
          f, { Ms.param_sizes = [| 2; 2 |]; Ms.values = [| 0; 0; 0; 1 |] };
          c1, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
          c2, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
          d1, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          d2, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          e, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
        ];
  } in
  let exp_model2 = {
    M.max_size = 2;
    M.symbs =
      hashtbl_of_list
        [
          f, { M.values = [| 0; 0; 0; 1 |] };
          c1, { M.values = [| 0 |] };
          c2, { M.values = [| 0 |] };
          d1, { M.values = [| 1 |] };
          d2, { M.values = [| 1 |] };
          e, { M.values = [| 0 |] };
        ];
  } in
  build_and_check_model prob ms_model2 exp_model2;

  let ms_model3 = {
    Ms.max_size = 3;
    Ms.symbs =
      hashtbl_of_list
        [
          f,
          {
            Ms.param_sizes = [| 2; 3 |];
            Ms.values = [| 1; 1; 1; 0; 2; 0 |];
          };
          c1, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          c2, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          d1, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
          d2, { Ms.param_sizes = [| |]; Ms.values = [| 2 |] };
          e, { Ms.param_sizes = [| |]; Ms.values = [| 2 |] };
        ];
  } in
  let exp_model3 = {
    M.max_size = 3;
    M.symbs =
      hashtbl_of_list
        [
          f, { M.values = [| 1; 1; 1; 0; 2; 0; 1; 1; 1 |] };
          c1, { M.values = [| 1 |] };
          c2, { M.values = [| 1 |] };
          d1, { M.values = [| 0 |] };
          d2, { M.values = [| 2 |] };
          e, { M.values = [| 2 |] };
        ];
  } in
  build_and_check_model prob ms_model3 exp_model3;

  let ms_model4 = {
    Ms.max_size = 4;
    Ms.symbs =
      hashtbl_of_list
        [
          f,
          {
            Ms.param_sizes = [| 2; 3 |];
            Ms.values = [| 2; 1; 0; 0; 3; 0 |];
          };
          c1, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          c2, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          d1, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
          d2, { Ms.param_sizes = [| |]; Ms.values = [| 2 |] };
          e, { Ms.param_sizes = [| |]; Ms.values = [| 3 |] };
        ];
  } in
  let exp_model4 = {
    M.max_size = 4;
    M.symbs =
      hashtbl_of_list
        [
          f,
          { M.values = [| 2; 1; 0; 1; 0; 3; 0; 3; 2; 1; 0; 1; 2; 1; 0; 1 |] };
          c1, { M.values = [| 1 |] };
          c2, { M.values = [| 1 |] };
          d1, { M.values = [| 0 |] };
          d2, { M.values = [| 2 |] };
          e, { M.values = [| 3 |] };
        ];
  } in
  build_and_check_model prob ms_model4 exp_model4

let test_comm_func_two_sorts_one_adeq_size () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let f = Symb.add db 2 in
  Symb.set_commutative db f true;
  let c = Symb.add db 0 in
  let d = Symb.add db 0 in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let z = T.Var 2 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x, y) = z, x = c, z <> d *)
    C.cl_lits = [
      T.mk_eq (T.Func (f, [| x; y |])) z;
      T.mk_eq x (T.Func (c, [| |]));
      T.mk_ineq z (T.Func (d, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model2 = {
    Ms.max_size = 2;
    Ms.symbs =
      hashtbl_of_list
        [
          f, { Ms.param_sizes = [| 2; 2 |]; Ms.values = [| 0; 1; 1; 1 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
        ];
  } in
  let exp_model2 = {
    M.max_size = 2;
    M.symbs =
      hashtbl_of_list
        [
          f, { M.values = [| 0; 1; 1; 1 |] };
          c, { M.values = [| 0 |] };
          d, { M.values = [| 1 |] };
        ];
  } in
  build_and_check_model prob ms_model2 exp_model2;

  let ms_model3 = {
    Ms.max_size = 3;
    Ms.symbs =
      hashtbl_of_list
        [
          f, { Ms.param_sizes = [| 2; 2 |]; Ms.values = [| 0; 2; 2; 2 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 2 |] };
        ];
  } in
  let exp_model3 = {
    M.max_size = 3;
    M.symbs =
      hashtbl_of_list
        [
          f, { M.values = [| 0; 2; 2; 2; 2; 2; 2; 2; 2 |] };
          c, { M.values = [| 0 |] };
          d, { M.values = [| 2 |] };
        ];
  } in
  build_and_check_model prob ms_model3 exp_model3

let suite =
  "Model suite" >:::
    [
      "nullary pred" >:: test_nullary_pred;
      "unary func, one sort, no adeq size" >::
        test_unary_func_one_sort_no_adeq_size;
      "binary pred, two sorts, no adeq size" >::
        test_binary_pred_two_sorts_no_adeq_size;
      "unary pred, one sort, adeq size" >::
        test_unary_pred_one_sort_adeq_size;
      "unary pred, one sort, adeq size 2" >::
        test_unary_pred_one_sort_adeq_size2;
      "binary func, two sorts, one adeq size" >::
        test_binary_func_two_sorts_one_adeq_size;
      "binary func, two sorts, one adeq size 2" >::
        test_binary_func_two_sorts_one_adeq_size2;
      "binary func, three sorts, two adeq sizes" >::
        test_binary_func_three_sorts_two_adeq_sizes;
      "comm func, two sorts, one adeq size" >::
        test_comm_func_two_sorts_one_adeq_size;
    ]
