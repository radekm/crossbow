(* Copyright (c) 2013 Radek Micek *)

open OUnit

module T = Term
module C = Clause

module Inst = Minisat_inst.Inst

(* injective: f(x) = f(y) -> x = y
   not surjective: f(x) <> c
*)
let test_only_infinite_model () =
  let prob = Prob.create () in
  let db = prob.Prob.symbols in
  let f =
    let s = Symb.add_anon_symb db 1 in
    fun a -> T.Func (s, [| a |]) in
  let c = T.Func (Symb.add_anon_symb db 0, [| |]) in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let z = T.Var 2 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x) <> z, z <> f(y), x = y *)
    C.cl_lits = [ T.mk_ineq (f x) z; T.mk_ineq z (f y); T.mk_eq x y ];
  } in
  let clause2 = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x) <> y, y <> c *)
    C.cl_lits = [ T.mk_ineq (f x) y; T.mk_ineq y c ];
  } in
  List.iter
    (BatDynArray.add prob.Prob.clauses)
    [clause; clause2];
  let sorts = Sorts.of_problem prob in

  let i = Inst.create prob sorts in
  for max_size = 1 to 15 do
    Inst.incr_max_size i;
    assert_equal Minisat_inst.Minisat_ex.Lfalse (Inst.solve i)
  done

(* couples:
   f(x) <> x
   f(x) = y -> f(y) = x
*)
let test_fin_models_even_size () =
  let prob = Prob.create () in
  let db = prob.Prob.symbols in
  let f =
    let s = Symb.add_anon_symb db 1 in
    fun a -> T.Func (s, [| a |]) in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let clause = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x) <> x *)
    C.cl_lits = [ T.mk_ineq (f x) x ];
  } in
  let clause2 = {
    C.cl_id = Prob.fresh_id prob;
    (* f(x) <> y, f(y) = x *)
    C.cl_lits = [ T.mk_ineq (f x) y; T.mk_eq (f y) x ];
  } in
  List.iter
    (BatDynArray.add prob.Prob.clauses)
    [clause; clause2];
  let sorts = Sorts.of_problem prob in

  let i = Inst.create prob sorts in
  for max_size = 1 to 15 do
    Inst.incr_max_size i;
    if max_size mod 2 = 0 then
      assert_equal Minisat_inst.Minisat_ex.Ltrue (Inst.solve i)
    else
      assert_equal Minisat_inst.Minisat_ex.Lfalse (Inst.solve i)
  done

let suite =
  "Minisat_inst suite" >:::
    [
      "only infinite model" >:: test_only_infinite_model;
      "finite models of even size" >:: test_fin_models_even_size;
    ]
