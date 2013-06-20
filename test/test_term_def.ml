(* Copyright (c) 2013 Radek Micek *)

open OUnit

module T = Term
module L = Lit
module C = Clause

let get_auxiliary_symbs db =
  let aux = ref [] in
  Symb.iter
    (fun s ->
      if Symb.auxiliary db s then
        aux := s :: !aux)
    db;
  BatList.sort !aux

let show_clauses cs = String.concat "\n" (BatList.map C.show cs)

let test_define_nested_terms () =
  let Symb.Wr db = Symb.create_db () in
  let p =
    let s = Symb.add_pred db 2 in
    fun a b -> L.lit (Sh.Pos, s, [| a; b |]) in
  let f =
    let s = Symb.add_func db 2 in
    fun a b -> T.func (s, [| a; b |]) in
  let g =
    let s = Symb.add_func db 1 in
    fun a -> T.func (s, [| a |]) in
  let c = T.func (Symb.add_func db 0, [| |]) in
  let d = T.func (Symb.add_func db 0, [| |]) in
  let x = T.var 0 in
  let y = T.var 1 in
  let z = T.var 2 in
  let clauses =
    BatDynArray.of_list
      [
        [
          L.mk_eq (f x (g (g c))) (f c d);
          L.neg (p x (f y (f x (g d))));
          L.mk_ineq (g (f d c)) z;
        ];
        [ L.mk_ineq z (g c) ];
      ] in
  let new_clauses = Term_def.define_ground_terms db clauses in
  let e1, e2, e3, e4, e5, e6 =
    match get_auxiliary_symbs db with
      | [e1; e2; e3; e4; e5; e6] -> (e1, e2, e3, e4, e5, e6)
      | _ -> failwith "test_define_nested_terms" in
  let e1 = T.func (e1, [| |]) in
  let e2 = T.func (e2, [| |]) in
  let e3 = T.func (e3, [| |]) in
  let e4 = T.func (e4, [| |]) in
  let e5 = T.func (e5, [| |]) in
  let e6 = T.func (e6, [| |]) in
  let exp_clauses =
    [
      [ L.mk_eq (g e4) e6 ];
      [ L.mk_eq (f c d) e5 ];
      [ L.mk_eq (f d c) e4 ];
      [ L.mk_eq (g d) e3 ];
      [ L.mk_eq (g e1) e2 ];
      [ L.mk_eq (g c) e1 ];
      [
        L.mk_eq (f x e2) e5;
        L.neg (p x (f y (f x e3)));
        L.mk_ineq z e6;
      ];
      [
        L.mk_ineq z e1;
      ];
    ] in
  assert_equal
    ~printer:show_clauses
    exp_clauses
    (BatDynArray.to_list new_clauses)

let test_define_terms_in_eqs () =
  let Symb.Wr db = Symb.create_db () in
  let f =
    let s = Symb.add_func db 2 in
    fun a b -> T.func (s, [| a; b |]) in
  let g =
    let s = Symb.add_func db 1 in
    fun a -> T.func (s, [| a |]) in
  let c1 = T.func (Symb.add_func db 0, [| |]) in
  let c2 = T.func (Symb.add_func db 0, [| |]) in
  let c3 = T.func (Symb.add_func db 0, [| |]) in
  let c4 = T.func (Symb.add_func db 0, [| |]) in
  let c5 = T.func (Symb.add_func db 0, [| |]) in
  let c6 = T.func (Symb.add_func db 0, [| |]) in
  let c7 = T.func (Symb.add_func db 0, [| |]) in
  let c8 = T.func (Symb.add_func db 0, [| |]) in
  let x = T.var 0 in
  let y = T.var 1 in
  let z = T.var 2 in
  let clauses =
    BatDynArray.of_list
      [
        [
          L.mk_eq c2 (g c1);
          L.mk_ineq x (g c3);
        ];
        [ L.mk_eq c4 (g c3) ];
        [
          L.mk_ineq (f x z) (g z);
          L.mk_eq (f c5 c4) (g y);
        ];
        [ L.mk_ineq (g c7) (g c6) ];
        [ L.mk_eq (g c8) (f c6 c6) ];
      ] in
  let new_clauses = Term_def.define_ground_terms db clauses in
  let e1, e2, e3, e4 =
    match get_auxiliary_symbs db with
      | [e1; e2; e3; e4] -> (e1, e2, e3, e4)
      | _ -> failwith "test_define_terms_in_eqs" in
  let e1 = T.func (e1, [| |]) in
  let e2 = T.func (e2, [| |]) in
  let e3 = T.func (e3, [| |]) in
  let e4 = T.func (e4, [| |]) in
  let exp_clauses =
    [
      [ L.mk_eq (g c6) e4 ];
      [ L.mk_eq (f c6 c6) e3 ];
      [ L.mk_eq (f c5 c4) e2 ];
      [ L.mk_eq (g c1) e1 ];
      [
        L.mk_eq c2 e1;
        L.mk_ineq x c4;
      ];
      [ L.mk_eq (g c3) c4 ];
      [
        L.mk_ineq (f x z) (g z);
        L.mk_eq (g y) e2;
      ];
      [ L.mk_ineq (g c7) e4 ];
      [ L.mk_eq (g c8) e3 ];
    ] in
  assert_equal
    ~printer:show_clauses
    exp_clauses
    (BatDynArray.to_list new_clauses)

let suite =
  "Term_def suite" >:::
    [
      "define nested terms" >:: test_define_nested_terms;
      "define terms in eqs" >:: test_define_terms_in_eqs;
    ]
