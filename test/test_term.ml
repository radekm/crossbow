(* Copyright (c) 2013 Radek Micek *)

open OUnit

module S = Symb
module T = Term

let test_contains1 () =
  let Symb.Wr db = Symb.create_db () in
  let f = Symb.add_func db 2 in
  let g = Symb.add_func db 2 in
  let i = Symb.add_func db 1 in
  let subterm = T.Func (f, [| T.Var 0; T.Func (i, [| T.Var 2 |]) |]) in
  let term =
    T.Func (g,
            [|
              T.Func (f, [| T.Var 0; T.Func (i, [| T.Var 1 |]) |]);
              T.Func (f, [| T.Func (i, [| T.Var 2 |]); T.Var 0 |]);
            |]
    ) in
  assert_bool "" (not (T.contains subterm term))

let test_contains2 () =
  let Symb.Wr db = Symb.create_db () in
  let f = Symb.add_func db 1 in
  let g = Symb.add_func db 2 in
  let subterm = T.Func (f, [| T.Var 1 |]) in
  let term =
    T.Func (g,
            [|
              T.Func (f, [| T.Func (f, [| T.Var 1 |]) |]);
              T.Var 1;
            |]
    ) in
  assert_bool "" (T.contains subterm term)

let test_iter () =
  let Symb.Wr db = Symb.create_db () in
  let f = Symb.add_func db 2 in
  let g = Symb.add_func db 1 in
  let x = T.Var 0 in
  let y = T.Var 1 in
  let z = T.Var 2 in
  let t1 = T.Func (f, [| x; z |]) in
  let t2 = T.Func (g, [| t1 |]) in
  (* f(g(f(x, z)), y) *)
  let term = T.Func (f, [| t2; y |]) in
  let subterms = ref [term; t2; t1; x; z; y] in
  let each_subterm t =
    assert_equal (List.hd !subterms) t;
    subterms := List.tl !subterms in
  T.iter each_subterm term;
  assert_equal [] !subterms

let test_normalize_comm () =
  let Symb.Wr db = Symb.create_db () in
  let f = Symb.add_func db 2 in
  let g = Symb.add_func db 2 in
  let h = Symb.add_func db 2 in
  Symb.set_commutative db f true;
  Symb.set_commutative db h true;
  let orig =
    T.Func (f,
            [|
              T.Func (h,
                      [|
                        T.Func (g, [| T.Var 3; T.Var 4 |]);
                        T.Func (g, [| T.Var 4; T.Var 3 |]);
                      |]
              );
              T.Func (h, [| T.Var 3; T.Var 1 |]);
            |]
    ) in
  let normalized =
    T.Func (f,
            [|
              T.Func (h, [| T.Var 1; T.Var 3 |]);
              T.Func (h,
                      [|
                        T.Func (g, [| T.Var 3; T.Var 4 |]);
                        T.Func (g, [| T.Var 4; T.Var 3 |]);
                      |]
              );
            |]
    ) in
  assert_equal normalized (T.normalize_comm db orig)


module IntSet = BatSet.IntSet

let test_vars () =
  let Symb.Wr db = Symb.create_db () in
  let f = Symb.add_func db 2 in
  let f a b = T.Func (f, [| a; b |]) in
  let c = Symb.add_func db 0 in
  let c = T.Func (c, [| |]) in
  let x = T.Var 1 in
  let y = T.Var 5 in
  let z = T.Var 2 in
  let term = f (f x (f c y)) (f (f x z) c) in
  let exp_vars = List.fold_right IntSet.add [1; 5; 2] IntSet.empty in
  assert_equal ~cmp:IntSet.equal exp_vars (T.vars term)

let suite =
  "Term suite" >:::
    [
      "contains 1" >:: test_contains1;
      "contains 2" >:: test_contains2;
      "iter" >:: test_iter;
      "normalize_comm" >:: test_normalize_comm;
      "vars" >:: test_vars;
    ]
