(* Copyright (c) 2013 Radek Micek *)

open OUnit

module Make (Solv : Sat_solver.S) : sig
  val suite : string -> test
end = struct

  let lit = Solv.to_lit Sat_solver.Pos
  let neg_lit = Solv.to_lit Sat_solver.Neg

  let test_new_vars_are_consecutive_ints () =
    let s = Solv.create () in
    let rec loop n prev_var =
      if n > 0 then begin
        let var = Solv.new_var s in
        assert_equal (prev_var+1) var;
        loop (n-1) var
      end in
    loop 1000 (Solv.new_var s)

  let test_to_lit_sane () =
    let s = Solv.create () in
    let vars = Array.init 50 (fun _ -> Solv.new_var s) in
    let pos_lits = Array.map lit vars in
    let neg_lits = Array.map neg_lit vars in
    (* to_var *)
    BatArray.iter2 (fun v l -> assert_equal v (Solv.to_var l)) vars pos_lits;
    BatArray.iter2 (fun v l -> assert_equal v (Solv.to_var l)) vars neg_lits;
    (* to_lit is injective *)
    let all_lits = Array.append pos_lits neg_lits in
    assert_equal
      (Array.length all_lits)
      (List.length (BatList.sort_unique compare (Array.to_list all_lits)))

  (* Problem without clauses has model. *)
  let test_all_vars_assigned_when_model_found () =
    let s = Solv.create () in
    let vars = Array.init 50 (fun _ -> Solv.new_var s) in
    assert_equal Sat_solver.Ltrue (Solv.solve s [| |]);
    Array.iter
      (fun v ->
        let b = Solv.model_value s v in
        assert_bool "" (b = Sat_solver.Ltrue || b = Sat_solver.Lfalse))
      vars

  let test_unsat_empty_clause () =
    let s = Solv.create () in
    let a = Solv.new_var s in
    (* a, ~a *)
    assert_bool "" (Solv.add_clause s [| lit a; neg_lit a |] 2);
    (* empty clause *)
    assert_bool "" (not (Solv.add_clause s [| lit a |] 0));
    assert_equal Sat_solver.Lfalse (Solv.solve s [| |])

  let test_unsat_zero_dec_level () =
    let s = Solv.create () in
    let a = Solv.new_var s in
    let b = Solv.new_var s in
    let c = Solv.new_var s in
    (* a, b, ~c *)
    assert_bool "" (Solv.add_clause s [| lit a; lit b; neg_lit c |] 3);
    (* ~a, ~c *)
    assert_bool "" (Solv.add_clause s [| neg_lit a; neg_lit c |] 2);
    (* a, c *)
    assert_bool "" (Solv.add_clause s [| lit a; lit c |] 2);
    (* ~b *)
    assert_bool "" (Solv.add_clause s [| neg_lit b |] 1);
    (* ~a *)
    assert_bool "" (not (Solv.add_clause s [| neg_lit a |] 1));
    assert_equal Sat_solver.Lfalse (Solv.solve s [| |])

  let generate_php s pigeons holes =
    (* phs.(p).(h) tells whether the pigeon p is in the hole h. *)
    let phs =
      Array.init pigeons
        (fun _ -> Array.init holes (fun _ -> Solv.new_var s)) in
    (* Each pigeon is in at least one hole. *)
    Array.iter
      (fun ph ->
        assert_bool "" (Solv.add_clause s (Array.map lit ph) holes))
      phs;
    (* Each pigeon is in at most one hole. *)
    Array.iter
      (fun ph ->
        for h = 0 to holes-1 do
          for i = h+1 to holes-1 do
            assert_bool
              ""
              (Solv.add_clause s [| neg_lit ph.(h); neg_lit ph.(i) |] 2)
          done
        done)
      phs;
    (* Each hole contains at most one pigeon. *)
    for h = 0 to holes-1 do
      for p = 0 to pigeons-1 do
        for q = p+1 to pigeons-1 do
          assert_bool
            ""
            (Solv.add_clause s
               [| neg_lit phs.(p).(h); neg_lit phs.(q).(h); |] 2)
        done
      done
    done;
    phs

  let test_unsat () =
    let s = Solv.create () in
    let _ = generate_php s 5 4 in
    assert_equal Sat_solver.Lfalse (Solv.solve s [| |])

  let test_sat () =
    let s = Solv.create () in
    let _ = generate_php s 5 5 in
    assert_equal Sat_solver.Ltrue (Solv.solve s [| |])

  let test_unsat_with_assumpts () =
    let s = Solv.create () in
    let phs = generate_php s 4 4  in
    (* No pigeon is in in the second hole. *)
    let assumpts = Array.map (fun ph -> neg_lit ph.(1)) phs in
    assert_equal Sat_solver.Lfalse (Solv.solve s assumpts);
    assert_equal Sat_solver.Ltrue (Solv.solve s [| |])

  let test_sat_with_assumpts () =
    let s = Solv.create () in
    let phs = generate_php s 4 5  in
    (* The first pigeon is in the third hole and the second pigeon
       is in the second hole.
    *)
    let assumpts = [| lit phs.(0).(2); lit phs.(1).(1) |] in
    assert_equal Sat_solver.Ltrue (Solv.solve s assumpts)

  let suite name =
    (name ^ " suite") >:::
      [
        "new vars are consecutive ints" >::
          test_new_vars_are_consecutive_ints;
        "to_lit is sane" >:: test_to_lit_sane;
        "all vars are assigned when model found" >::
          test_all_vars_assigned_when_model_found;
        "unsatisfiable by empty clause" >:: test_unsat_empty_clause;
        "unsatisfiable at zero decision level" >:: test_unsat_zero_dec_level;
        "unsat" >:: test_unsat;
        "sat" >:: test_sat;
        "unsat with assumptions" >:: test_unsat_with_assumpts;
        "sat with assumptions" >:: test_sat_with_assumpts;
      ]

end
