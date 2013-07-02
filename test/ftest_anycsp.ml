(* Copyright (c) 2013 Radek Micek *)

open OUnit

let (|>) = BatPervasives.(|>)

module Make (Solv : Csp_solver.S) : sig
  val suite : string -> test
end = struct

  let test_linear () =
    let s = Solv.create 1 in
    let x = Solv.new_int_var s 6 in
    Solv.linear s [| x |] [| 3 |] 12;
    assert_equal Sh.Ltrue (Solv.solve s);
    assert_equal 4 (Solv.int_value s x);
    assert_equal Sh.Lfalse (Solv.solve s)

  let test_linear2 () =
    let s = Solv.create 1 in
    let x = Solv.new_int_var s 7 in
    let y = Solv.new_int_var s 7 in
    Solv.linear s [| x; y |] [| ~-1; 5 |] 13;
    assert_equal Sh.Ltrue (Solv.solve s);
    assert_equal 2 (Solv.int_value s x);
    assert_equal 3 (Solv.int_value s y);
    assert_equal Sh.Lfalse (Solv.solve s)

  let test_bool_element () =
    let s = Solv.create 1 in
    let x = Solv.new_bool_var s in
    let y1 = Solv.new_tmp_bool_var s in
    let y2 = Solv.new_tmp_bool_var s in
    let vars = [| y1; y2; x |] in
    let arr = Solv.new_bool_var_array s vars in
    Solv.clause s [| |] [| x |];
    Solv.clause s [| y1 |] [| |];
    Solv.clause s [| y2 |] [| |];
    (* Index of x in the array. *)
    let i = Solv.new_int_var s 3 in
    Solv.bool_element s arr i x;
    assert_equal Sh.Ltrue (Solv.solve s);
    assert_equal 0 (Solv.bool_value s x);
    assert_equal 2 (Solv.int_value s i);
    assert_equal Sh.Lfalse (Solv.solve s)

  let test_int_element () =
    let s = Solv.create 1 in
    let y1 = Solv.new_tmp_int_var s 7 in
    let y2 = Solv.new_tmp_int_var s 7 in
    let x1 = Solv.new_int_var s 5 in
    let x2 = Solv.new_int_var s 5 in
    let vars = [| x1; y1; x2; y2 |] in
    let arr = Solv.new_int_var_array s vars in
    (* y2 = x2. *)
    let i = Solv.new_tmp_int_var s 4 in
    let b = Solv.new_tmp_bool_var s in
    Solv.eq_var_const s i 3 b;
    Solv.clause s [| b |] [| |];
    Solv.int_element s arr i x2;
    (* y1 = x1. *)
    let j = Solv.new_tmp_int_var s 4 in
    let b' = Solv.new_tmp_bool_var s in
    Solv.eq_var_const s j 1 b';
    Solv.clause s [| b' |] [| |];
    Solv.int_element s arr j x1;
    Solv.linear s [| y1; y2 |] [| 7; 3 |] 31;
    assert_equal Sh.Ltrue (Solv.solve s);
    assert_equal 4 (Solv.int_value s x1);
    assert_equal 1 (Solv.int_value s x2);
    assert_equal Sh.Lfalse (Solv.solve s)

  let test_eq_var_var_eq_var_const () =
    let s = Solv.create 1 in
    let yes = Solv.new_tmp_bool_var s in
    let no = Solv.new_tmp_bool_var s in
    Solv.clause s [| yes |] [| |];
    Solv.clause s [| |] [| no |];
    let x1 = Solv.new_int_var s 2 in
    let x2 = Solv.new_int_var s 3 in
    let x3 = Solv.new_int_var s 4 in
    let x4 = Solv.new_int_var s 5 in
    let x5 = Solv.new_int_var s 6 in
    Solv.eq_var_const s x1 0 no;
    Solv.eq_var_const s x2 0 no;
    Solv.eq_var_var s x1 x2 no;
    Solv.eq_var_var s x3 x4 no;
    Solv.eq_var_const s x5 4 yes;
    Solv.eq_var_var s x2 x3 yes;
    Solv.eq_var_var s x4 x5 yes;
    assert_equal Sh.Ltrue (Solv.solve s);
    assert_equal 1 (Solv.int_value s x1);
    assert_equal 2 (Solv.int_value s x2);
    assert_equal 2 (Solv.int_value s x3);
    assert_equal 4 (Solv.int_value s x4);
    assert_equal 4 (Solv.int_value s x5);
    assert_equal Sh.Lfalse (Solv.solve s)

  let test_clause_empty () =
    let s = Solv.create 1 in
    Solv.clause s [| |] [| |];
    assert_equal Sh.Lfalse (Solv.solve s);
    assert_equal Sh.Lfalse (Solv.solve s)

  let test_clause () =
    let s = Solv.create 1 in
    let p = Solv.new_bool_var s in
    let q = Solv.new_bool_var s in
    let r = Solv.new_bool_var s in
    Solv.clause s [| p |] [| r; q |];
    Solv.clause s [| r; q |] [| |];
    Solv.clause s [| |] [| p |];
    assert_equal Sh.Ltrue (Solv.solve s);
    let p1 = Solv.bool_value s p in
    let q1 = Solv.bool_value s q in
    let r1 = Solv.bool_value s r in
    assert_equal Sh.Ltrue (Solv.solve s);
    let p2 = Solv.bool_value s p in
    let q2 = Solv.bool_value s q in
    let r2 = Solv.bool_value s r in
    assert_equal Sh.Lfalse (Solv.solve s);
    assert_equal 0 p1;
    assert_equal 0 p2;
    assert_bool "" (q1 = 0 || q1 = 1);
    assert_bool "" (q2 = 0 || q2 = 1);
    assert_bool "" (r1 = 0 || r1 = 1);
    assert_bool "" (r2 = 0 || r2 = 1);
    assert_bool "" (q1 = r2);
    assert_bool "" (r1 = q2);
    assert_bool "" (q1 <> r1)

  let test_all_different () =
    let s = Solv.create 1 in
    let y1 = Solv.new_tmp_int_var s 3 in
    let x1 = Solv.new_int_var s 2 in
    let x2 = Solv.new_int_var s 2 in
    let x3 = Solv.new_int_var s 4 in
    let y2 = Solv.new_tmp_int_var s 5 in
    Solv.all_different s [| x1; x2; x3; y1; y2 |];
    assert_equal Sh.Ltrue (Solv.solve s);
    assert_equal 0 (Solv.int_value s x1);
    assert_equal 1 (Solv.int_value s x2);
    assert_equal 3 (Solv.int_value s x3);
    assert_equal Sh.Ltrue (Solv.solve s);
    assert_equal 1 (Solv.int_value s x1);
    assert_equal 0 (Solv.int_value s x2);
    assert_equal 3 (Solv.int_value s x3);
    assert_equal Sh.Lfalse (Solv.solve s)

  let test_pythagorean_triples () =
    let s = Solv.create 1 in
    let yes = Solv.new_tmp_bool_var s in
    Solv.clause s [| yes |] [| |];
    let n = 10 in
    let sq_dom = n * n + 1 in
    (* Variables equal to squares of positive numbers 1..n. *)
    let sq_vars =
      Array.init n (fun i ->
        let x = Solv.new_tmp_int_var s sq_dom in
        Solv.eq_var_const s x ((i + 1) * (i + 1)) yes;
        x) in
    let sq_arr = Solv.new_int_var_array s sq_vars in
    (* x, y, z are squares of positive numbers. *)
    let i = Solv.new_int_var s n in
    let x = Solv.new_int_var s sq_dom in
    Solv.int_element s sq_arr i x;
    let j = Solv.new_int_var s n in
    let y = Solv.new_int_var s sq_dom in
    Solv.int_element s sq_arr j y;
    let k = Solv.new_int_var s n in
    let z = Solv.new_int_var s sq_dom in
    Solv.int_element s sq_arr k z;
    (* x <= y (i.e. j - i - tmp = 0). *)
    let tmp = Solv.new_tmp_int_var s n in
    Solv.linear s [| i; j; tmp |] [| ~-1; 1; ~-1 |] 0;
    (* x + y = z. *)
    Solv.linear s [| x; y; z |] [| 1; 1; ~-1 |] 0;
    assert_equal Sh.Ltrue (Solv.solve s);
    assert_equal 9 (Solv.int_value s x);
    assert_equal 16 (Solv.int_value s y);
    assert_equal 25 (Solv.int_value s z);
    assert_equal Sh.Ltrue (Solv.solve s);
    assert_equal 36 (Solv.int_value s x);
    assert_equal 64 (Solv.int_value s y);
    assert_equal 100 (Solv.int_value s z);
    assert_equal Sh.Lfalse (Solv.solve s)

  let suite name =
    (name ^ " suite") >:::
      [
        "linear" >:: test_linear;
        "linear 2" >:: test_linear2;
        "bool_element" >:: test_bool_element;
        "int_element" >:: test_int_element;
        "eq_var_var, eq_var_const" >:: test_eq_var_var_eq_var_const;
        "clause - empty" >:: test_clause_empty;
        "clause" >:: test_clause;
        "all_different" >:: test_all_different;
        "pythagorean triples" >:: test_pythagorean_triples;
      ]

end
