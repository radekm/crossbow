(* Copyright (c) 2013 Radek Micek *)

open OUnit

module T = Term

let test_mk_eq () =
  let db = Symb.create_db () in
  let f = Symb.add db 2 in
  let l = T.Func (f, [| T.Var 1; T.Var 2 |]) in
  let r = T.Var 0 in
  assert_equal
    (T.Func (Symb.sym_eq, [| l; r |]))
    (T.mk_eq l r)

let test_mk_ineq () =
  let db = Symb.create_db () in
  let c = Symb.add db 0 in
  let d = Symb.add db 0 in
  let c = T.Func (c, [| |]) in
  let d = T.Func (d, [| |]) in
  assert_equal
    (T.Func (Symb.sym_not,
           [| T.Func (Symb.sym_eq, [| c; d |]) |]))
    (T.mk_ineq c d)

let test_contains1 () =
  let db = Symb.create_db () in
  let f = Symb.add db 2 in
  let g = Symb.add db 2 in
  let i = Symb.add db 1 in
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
  let db = Symb.create_db () in
  let f = Symb.add db 1 in
  let g = Symb.add db 2 in
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
  let db = Symb.create_db () in
  let f = Symb.add db 2 in
  let g = Symb.add db 1 in
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

let test_pickp1 () =
  let db = Symb.create_db () in
  let f = Symb.add db 1 in
  let cond p t = match p, t with
    | Some _, T.Func (s, [| (T.Var _) as x |]) when s = f -> Some x
    | _, _ -> None in
  let term = T.Func (f, [| T.Var 42 |]) in
  assert_equal None (T.pickp cond term)

let test_pickp2 () =
  let db = Symb.create_db () in
  let f = Symb.add db 1 in
  let cond p t = match p, t with
    | Some _, T.Func (s, [| (T.Var _) as x |]) when s = f -> Some x
    | _, _ -> None in
  let term =
    T.Func (f,
            [| T.Func (f, [| T.Var 42 |]) |]
    ) in
  assert_equal (Some (T.Var 42)) (T.pickp cond term)

let test_normalize_comm () =
  let db = Symb.create_db () in
  let f = Symb.add db 2 in
  let g = Symb.add db 2 in
  let h = Symb.add db 2 in
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

let test_replace () =
  let db = Symb.create_db () in
  let f = Symb.add db 2 in
  let g = Symb.add db 1 in
  let c = Symb.add db 0 in
  let sub_old = T.Func (f, [| T.Func (c, [| |]); T.Var 0 |]) in
  let sub_new = T.Func (g, [| T.Func (c, [| |]) |]) in
  let mk_term sub =
    Term.mk_ineq
      (T.Func (g,
               [|
                 T.Func (f,
                         [|
                           sub;
                           T.Func (f, [| T.Func (c, [| |]); T.Var 1 |]);
                         |]
                 );
               |]))
      (T.Func (g, [| sub |])) in
  assert_equal
    (mk_term sub_new)
    (Term.replace sub_old sub_new (mk_term sub_old))

module IntSet = BatSet.IntSet

let test_vars () =
  let db = Symb.create_db () in
  let f = Symb.add db 2 in
  let f a b = T.Func (f, [| a; b |]) in
  let c = Symb.add db 0 in
  let c = T.Func (c, [| |]) in
  let x = T.Var 1 in
  let y = T.Var 5 in
  let z = T.Var 2 in
  let term = f (f x (f c y)) (f (f x z) c) in
  let exp_vars = List.fold_right IntSet.add [1; 5; 2] IntSet.empty in
  assert_equal ~cmp:IntSet.equal exp_vars (T.vars term)

let test_vars_of_many () =
  let db = Symb.create_db () in
  let f = Symb.add db 3 in
  let f a b c = T.Func (f, [| a; b; c |]) in
  let g = Symb.add db 2 in
  let g a b = T.Func (g, [| a; b |]) in
  let c = Symb.add db 0 in
  let c = T.Func (c, [| |]) in
  let x1 = T.Var 3 in
  let x2 = T.Var 8 in
  let x3 = T.Var 0 in
  let x4 = T.Var 9 in
  let x5 = T.Var 15 in
  let y = T.Var 2 in
  let z = T.Var 1 in
  let terms = [
    T.mk_ineq (f (g c x1) x3 x3) x2;
    T.mk_eq x4 (f (g x5 c) c c);
    T.mk_eq y (g z z);
  ] in
  let exp_vars =
    List.fold_right IntSet.add [3; 8; 0; 9; 15; 2; 1] IntSet.empty in
  assert_equal ~cmp:IntSet.equal exp_vars (T.vars_of_many terms)

let suite =
  "Term suite" >:::
    [
      "mk_eq" >:: test_mk_eq;
      "mk_ineq" >:: test_mk_ineq;
      "contains 1" >:: test_contains1;
      "contains 2" >:: test_contains2;
      "iter" >:: test_iter;
      "pickp 1" >:: test_pickp1;
      "pickp 2" >:: test_pickp2;
      "normalize_comm" >:: test_normalize_comm;
      "replace" >:: test_replace;
      "vars" >:: test_vars;
      "vars_of_many" >:: test_vars_of_many;
    ]
