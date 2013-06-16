(* Copyright (c) 2013 Radek Micek *)

open OUnit

module C = Clause2
module T = Term
module L = Lit
module Ms = Ms_model
module M = Model

let map_of_list xs = Symb.Map.of_enum (BatList.enum xs)

let show_model m =
  let b = Buffer.create 128 in
  Printf.bprintf b "Max size: %d\n" m.M.max_size;
  Symb.Map.iter
    (fun symb table ->
      Printf.bprintf b "Symbol %2d: [" (Symb.id_to_int symb);
      Array.iter (Printf.bprintf b " %2d") table.M.values;
      Printf.bprintf b " ]\n")
    m.M.symbs;
  Buffer.contents b

let build_and_check_model prob ms_model exp_model =
  let sorts = Sorts.of_problem prob in
  let model = M.of_ms_model ms_model sorts in
  assert_equal ~cmp:M.equal ~printer:show_model exp_model model

let test_nullary_pred () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 0 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* p *)
    C.cl_lits = [ L.lit (Sh.Pos, p, [| |]) ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model = {
    Ms.max_size = 1;
    Ms.symbs =
      map_of_list
        [
          p, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
        ];
  } in
  let exp_model = {
    M.max_size = 1;
    M.symbs =
      map_of_list
        [
          p, { M.values = [| 1 |] };
        ];
  } in
  build_and_check_model prob ms_model exp_model

let test_unary_func_one_sort_no_adeq_size () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let f = Symb.add_func db 1 in
  let x = T.var 0 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x) <> x *)
    C.cl_lits = [ L.mk_ineq (T.func (f, [| x |])) x ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list
        [
          f, { Ms.param_sizes = [| 3 |]; Ms.values = [| 1; 2; 1 |] };
        ];
  } in
  let exp_model = {
    M.max_size = 3;
    M.symbs =
      map_of_list
        [
          f, { M.values = [| 1; 2; 1 |] };
        ];
  } in
  build_and_check_model prob ms_model exp_model

let test_binary_pred_two_sorts_no_adeq_size () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 2 in
  let x = T.var 0 in
  let y = T.var 1 in
  let u = T.var 2 in
  let v = T.var 3 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* p(x, y), x = u, y = v *)
    C.cl_lits = [
      L.lit (Sh.Pos, p, [| x; y; |]);
      L.mk_eq x u;
      L.mk_eq y v;
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model = {
    Ms.max_size = 2;
    Ms.symbs =
      map_of_list
        [
          p, { Ms.param_sizes = [| 2; 2 |]; Ms.values = [| 1; 1; 1; 1 |] };
        ];
  } in
  let exp_model = {
    M.max_size = 2;
    M.symbs =
      map_of_list
        [
          p, { M.values = [| 1; 1; 1; 1 |] };
        ];
  } in
  build_and_check_model prob ms_model exp_model

let test_unary_pred_one_sort_adeq_size () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 1 in
  let c = Symb.add_func db 0 in
  let d = Symb.add_func db 0 in
  let x = T.var 0 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* ~p(x), x = c, x = d *)
    C.cl_lits = [
      L.neg (L.lit (Sh.Pos, p, [| x |]));
      L.mk_eq x (T.func (c, [| |]));
      L.mk_eq x (T.func (d, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model2 = {
    Ms.max_size = 2;
    Ms.symbs =
      map_of_list
        [
          p, { Ms.param_sizes = [| 2 |]; Ms.values = [| 0; 1 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
        ];
  } in
  let exp_model2 = {
    M.max_size = 2;
    M.symbs =
      map_of_list
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
      map_of_list
        [
          p, { Ms.param_sizes = [| 3 |]; Ms.values = [| 0; 1; 1 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 2 |] };
        ];
  } in
  let exp_model3 = {
    M.max_size = 3;
    M.symbs =
      map_of_list
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
      map_of_list
        [
          p, { Ms.param_sizes = [| 3 |]; Ms.values = [| 0; 1; 1 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 2 |] };
        ];
  } in
  let exp_model4 = {
    M.max_size = 4;
    M.symbs =
      map_of_list
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
  let p = Symb.add_pred db 1 in
  let c = Symb.add_func db 0 in
  let d = Symb.add_func db 0 in
  let x = T.var 0 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* ~p(x), x <> c, x <> d *)
    C.cl_lits = [
      L.neg (L.lit (Sh.Pos, p, [| x |]));
      L.mk_ineq x (T.func (c, [| |]));
      L.mk_ineq x (T.func (d, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model2 = {
    Ms.max_size = 2;
    Ms.symbs =
      map_of_list
        [
          p, { Ms.param_sizes = [| 2 |]; Ms.values = [| 1; 0 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
        ];
  } in
  let exp_model2 = {
    M.max_size = 2;
    M.symbs =
      map_of_list
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
      map_of_list
        [
          p, { Ms.param_sizes = [| 2 |]; Ms.values = [| 1; 0 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
        ];
  } in
  let exp_model3 = {
    M.max_size = 3;
    M.symbs =
      map_of_list
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
  let f = Symb.add_func db 2 in
  let c = Symb.add_func db 0 in
  let x = T.var 0 in
  let y = T.var 1 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x, y) = y, x = c *)
    C.cl_lits = [
      L.mk_eq (T.func (f, [| x; y |])) y;
      L.mk_eq x (T.func (c, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model1 = {
    Ms.max_size = 1;
    Ms.symbs =
      map_of_list
        [
          f, { Ms.param_sizes = [| 1; 1 |]; Ms.values = [| 0 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
        ];
  } in
  let exp_model1 = {
    M.max_size = 1;
    M.symbs =
      map_of_list
        [
          f, { M.values = [| 0 |] };
          c, { M.values = [| 0 |] };
        ];
  } in
  build_and_check_model prob ms_model1 exp_model1;

  let ms_model2 = {
    Ms.max_size = 2;
    Ms.symbs =
      map_of_list
        [
          f, { Ms.param_sizes = [| 2; 2 |]; Ms.values = [| 1; 0; 0; 1 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
        ];
  } in
  let exp_model2 = {
    M.max_size = 2;
    M.symbs =
      map_of_list
        [
          f, { M.values = [| 1; 0; 0; 1 |] };
          c, { M.values = [| 0 |] };
        ];
  } in
  build_and_check_model prob ms_model2 exp_model2;

  let ms_model3 = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list
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
      map_of_list
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
  let f = Symb.add_func db 2 in
  let c = Symb.add_func db 0 in
  let x = T.var 0 in
  let y = T.var 1 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x, y) = x, y = c *)
    C.cl_lits = [
      L.mk_eq (T.func (f, [| x; y |])) x;
      L.mk_eq y (T.func (c, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model1 = {
    Ms.max_size = 1;
    Ms.symbs =
      map_of_list
        [
          f, { Ms.param_sizes = [| 1; 1 |]; Ms.values = [| 0 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
        ];
  } in
  let exp_model1 = {
    M.max_size = 1;
    M.symbs =
      map_of_list
        [
          f, { M.values = [| 0 |] };
          c, { M.values = [| 0 |] };
        ];
  } in
  build_and_check_model prob ms_model1 exp_model1;

  let ms_model2 = {
    Ms.max_size = 2;
    Ms.symbs =
      map_of_list
        [
          f, { Ms.param_sizes = [| 2; 2 |]; Ms.values = [| 1; 0; 0; 1 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
        ];
  } in
  let exp_model2 = {
    M.max_size = 2;
    M.symbs =
      map_of_list
        [
          f, { M.values = [| 1; 0; 0; 1 |] };
          c, { M.values = [| 0 |] };
        ];
  } in
  build_and_check_model prob ms_model2 exp_model2;

  let ms_model3 = {
    Ms.max_size = 3;
    Ms.symbs =
      map_of_list
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
      map_of_list
        [
          f, { M.values = [| 2; 0; 0; 2; 1; 1; 1; 2; 2 |] };
          c, { M.values = [| 0 |] };
        ];
  } in
  build_and_check_model prob ms_model3 exp_model3

let test_binary_func_three_sorts_two_adeq_sizes () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let f = Symb.add_func db 2 in
  let c1 = Symb.add_func db 0 in
  let c2 = Symb.add_func db 0 in
  let d1 = Symb.add_func db 0 in
  let d2 = Symb.add_func db 0 in
  let e = Symb.add_func db 0 in
  let x = T.var 0 in
  let y = T.var 1 in
  let z = T.var 2 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x, y) = z, x <> c1, x <> c2, y = d1, y = d2, z <> e *)
    C.cl_lits = [
      L.mk_eq (T.func (f, [| x; y |])) z;
      L.mk_ineq x (T.func (c1, [| |]));
      L.mk_ineq x (T.func (c2, [| |]));
      L.mk_eq y (T.func (d1, [| |]));
      L.mk_eq y (T.func (d2, [| |]));
      L.mk_ineq z (T.func (e, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model1 = {
    Ms.max_size = 1;
    Ms.symbs =
      map_of_list
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
      map_of_list
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
      map_of_list
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
      map_of_list
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
      map_of_list
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
      map_of_list
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
      map_of_list
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
      map_of_list
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
  let f = Symb.add_func db 2 in
  Symb.set_commutative db f true;
  let c = Symb.add_func db 0 in
  let d = Symb.add_func db 0 in
  let x = T.var 0 in
  let y = T.var 1 in
  let z = T.var 2 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x, y) = z, x = c, z <> d *)
    C.cl_lits = [
      L.mk_eq (T.func (f, [| x; y |])) z;
      L.mk_eq x (T.func (c, [| |]));
      L.mk_ineq z (T.func (d, [| |]));
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;

  let ms_model2 = {
    Ms.max_size = 2;
    Ms.symbs =
      map_of_list
        [
          f, { Ms.param_sizes = [| 2; 2 |]; Ms.values = [| 0; 1; 1; 1 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 1 |] };
        ];
  } in
  let exp_model2 = {
    M.max_size = 2;
    M.symbs =
      map_of_list
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
      map_of_list
        [
          f, { Ms.param_sizes = [| 2; 2 |]; Ms.values = [| 0; 2; 2; 2 |] };
          c, { Ms.param_sizes = [| |]; Ms.values = [| 0 |] };
          d, { Ms.param_sizes = [| |]; Ms.values = [| 2 |] };
        ];
  } in
  let exp_model3 = {
    M.max_size = 3;
    M.symbs =
      map_of_list
        [
          f, { M.values = [| 0; 2; 2; 2; 2; 2; 2; 2; 2 |] };
          c, { M.values = [| 0 |] };
          d, { M.values = [| 2 |] };
        ];
  } in
  build_and_check_model prob ms_model3 exp_model3

let test_canonize () =
  let Symb.Wr db = Symb.create_db () in
  let c = Symb.add_func db 0 in
  let f = Symb.add_func db 2 in
  let p = Symb.add_pred db 0 in
  let q = Symb.add_pred db 1 in

  let model = {
    M.max_size = 3;
    M.symbs = map_of_list [
      c, { M.values = [| 2 |] };
      f, { M.values = [| 1; 2; 2; 0; 1; 0; 0; 0; 2 |] };
      p, { M.values = [| 1 |] };
      q, { M.values = [| 0; 1; 1 |] };
    ];
  } in

  (* permutation: 0 2 1 *)
  let model2 = {
    M.max_size = 3;
    M.symbs = map_of_list [
      c, { M.values = [| 1 |] };
      f, { M.values = [| 2; 1; 1; 0; 1; 0; 0; 0; 2 |] };
      p, { M.values = [| 1 |] };
      q, { M.values = [| 0; 1; 1 |] };
    ];
  } in

  (* permutation: 1 0 2 *)
  let model3 = {
    M.max_size = 3;
    M.symbs = map_of_list [
      c, { M.values = [| 2 |] };
      f, { M.values = [| 0; 1; 1; 2; 0; 2; 1; 1; 2 |] };
      p, { M.values = [| 1 |] };
      q, { M.values = [| 1; 0; 1 |] };
    ];
  } in

  (* permutation: 1 2 0 *)
  let model4 = {
    M.max_size = 3;
    M.symbs = map_of_list [
      c, { M.values = [| 0 |] };
      f, { M.values = [| 0; 1; 1; 0; 2; 0; 1; 1; 2 |] };
      p, { M.values = [| 1 |] };
      q, { M.values = [| 1; 0; 1 |] };
    ];
  } in

  (* permutation: 2 0 1 *)
  let model5 = {
    M.max_size = 3;
    M.symbs = map_of_list [
      c, { M.values = [| 1 |] };
      f, { M.values = [| 0; 2; 2; 2; 1; 2; 1; 1; 0 |] };
      p, { M.values = [| 1 |] };
      q, { M.values = [| 1; 1; 0 |] };
    ];
  } in

  (* permutation: 2 1 0 *)
  let model6 = {
    M.max_size = 3;
    M.symbs = map_of_list [
      c, { M.values = [| 0 |] };
      f, { M.values = [| 0; 2; 2; 2; 1; 2; 0; 0; 1 |] };
      p, { M.values = [| 1 |] };
      q, { M.values = [| 1; 1; 0 |] };
    ];
  } in

  let all_models = [model; model2; model3; model4; model5; model6] in
  let m = M.canonize model in
  assert_bool "" (List.exists (M.equal m) all_models);
  List.iter
    (fun m' -> assert_equal ~cmp:M.equal m (M.canonize m'))
    all_models

let test_all_of_ms_model () =
  let Prob.Wr prob = Prob.create () in
  let db = prob.Prob.symbols in
  let p = Symb.add_pred db 1 in
  let f = Symb.add_func db 2 in
  let c = Symb.add_func db 0 in
  let d = Symb.add_func db 0 in
  let x = T.var 0 in
  let y = T.var 1 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x, x) = y, x = c, x <> d *)
    C.cl_lits = [
      L.mk_eq (T.func (f, [| x; x |])) y;
      L.mk_eq x (T.func (c, [| |]));
      L.mk_ineq x (T.func (d, [| |]));
      L.lit (Sh.Neg, p, [| y |]);
    ];
  } in
  BatDynArray.add prob.Prob.clauses clause;
  let sorts = Sorts.of_problem prob in

  let ms_model = {
    Ms.max_size = 3;
    Ms.symbs = map_of_list [
      p, { Ms.param_sizes = [| 3 |]; Ms.values = [| 1; 0; 0 |] };
      f, {
        Ms.param_sizes = [| 3; 3 |];
        Ms.values = [| 2; 1; 0; 0; 1; 1; 1; 2; 0 |];
      };
      c, { Ms.param_sizes = [| |]; Ms.values = [| 2 |] };
      d, { Ms.param_sizes = [| |]; Ms.values = [| 2 |] };
    ];
  } in

  let model = {
    M.max_size = 3;
    M.symbs = map_of_list [
      p, { M.values = [| 1; 0; 0 |] };
      f, { M.values = [| 2; 1; 0; 0; 1; 1; 1; 2; 0 |] };
      c, { M.values = [| 2 |] };
      d, { M.values = [| 2 |] };
     ];
  } in

  (* permutation: 0 2 1 *)
  let model2 = {
    M.max_size = 3;
    M.symbs = map_of_list [
      p, { M.values = [| 1; 0; 0 |] };
      f, { M.values = [| 1; 2; 0; 0; 2; 2; 2; 1; 0 |] };
      c, { M.values = [| 2 |] };
      d, { M.values = [| 2 |] };
     ];
  } in

  (* permutation: 1 0 2 *)
  let model3 = {
    M.max_size = 3;
    M.symbs = map_of_list [
      p, { M.values = [| 0; 1; 0 |] };
      f, { M.values = [| 2; 0; 1; 1; 0; 0; 0; 2; 1 |] };
      c, { M.values = [| 2 |] };
      d, { M.values = [| 2 |] };
     ];
  } in

  (* permutation: 1 2 0 *)
  let model4 = {
    M.max_size = 3;
    M.symbs = map_of_list [
      p, { M.values = [| 0; 1; 0 |] };
      f, { M.values = [| 0; 2; 1; 1; 2; 2; 2; 0; 1 |] };
      c, { M.values = [| 2 |] };
      d, { M.values = [| 2 |] };
     ];
  } in

  (* permutation: 2 0 1 *)
  let model5 = {
    M.max_size = 3;
    M.symbs = map_of_list [
      p, { M.values = [| 0; 0; 1 |] };
      f, { M.values = [| 1; 0; 2; 2; 0; 0; 0; 1; 2 |] };
      c, { M.values = [| 2 |] };
      d, { M.values = [| 2 |] };
     ];
  } in

  (* permutation: 2 1 0 *)
  let model6 = {
    M.max_size = 3;
    M.symbs = map_of_list [
      p, { M.values = [| 0; 0; 1 |] };
      f, { M.values = [| 0; 1; 2; 2; 1; 1; 1; 0; 2 |] };
      c, { M.values = [| 2 |] };
      d, { M.values = [| 2 |] };
     ];
  } in

  let all_models = [model; model2; model3; model4; model5; model6] in
  let canonized_models =
    BatList.sort_unique M.compare (BatList.map M.canonize all_models) in
  let models = M.all_of_ms_model ms_model sorts in

  assert_equal 6 (List.length canonized_models);
  assert_equal 6 (BatSet.cardinal models);
  List.iter (fun m -> assert_bool "" (BatSet.mem m models)) canonized_models

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
      "canonize" >:: test_canonize;
      "all_of_ms_model" >:: test_all_of_ms_model;
    ]
