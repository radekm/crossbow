(* Copyright (c) 2013 Radek Micek *)

open OUnit

let (|>) = BatPervasives.(|>)

module Make (Solv : Sat_solver.S) : sig
  val suite : string -> test
end = struct

  let lit = Solv.to_lit Sh.Pos
  let neg_lit = Solv.to_lit Sh.Neg

  let base_dir = "test/data/cnf/"

  (* Initialize parser from CNF file. *)
  let of_cnf_file file =
    let lines =
      BatFile.lines_of file
      |> BatEnum.map BatString.trim
      |> BatEnum.filter (fun line -> line <> "")
      |> BatEnum.filter (fun line -> BatString.starts_with line "c" |> not)
      |> BatList.of_enum in
    match lines with
      | [] -> failwith "of_cnf_file: no header"
      | l :: ls ->
          match BatString.nsplit l " " with
            | ["p"; "cnf"; nvars_str; _] ->
                let solver = Solv.create () in
                let nvars = int_of_string nvars_str in
                let x =
                  if nvars <= 0 then
                    ~-1
                  else begin
                    let x = Solv.new_var solver in
                    for v = 2 to nvars do
                      Solv.new_var solver |> ignore
                    done;
                    x
                  end in
                let to_lit lit_str =
                  (* In case of two or more consecutive spaces. *)
                  if lit_str = "" then
                    None
                  else
                    let lit = int_of_string lit_str in
                    if lit = 0 then
                      None
                    else
                      let sign =
                        if lit < 0
                        then Sh.Neg
                        else Sh.Pos in
                      let var = x + abs lit - 1 in
                      Some (Solv.to_lit sign var) in
                let add_clause clause_str =
                  let lits =
                    BatString.nsplit clause_str " "
                    |> BatList.filter_map to_lit
                    |> Earray.of_list in
                  let n = Earray.length lits in
                  Solv.add_clause solver lits n |> ignore in
                List.iter add_clause ls;
                solver
            | _ -> failwith "of_cnf_file: invalid header"

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
    assert_equal Sh.Ltrue (Solv.solve s [| |]);
    Array.iter
      (fun v ->
        let b = Solv.model_value s v in
        assert_bool "" (b = Sh.Ltrue || b = Sh.Lfalse))
      vars

  let test_unsat_empty_clause () =
    let s = Solv.create () in
    let a = Solv.new_var s in
    (* a, ~a *)
    assert_bool "" (Solv.add_clause s [| lit a; neg_lit a |] 2);
    (* empty clause *)
    assert_bool "" (not (Solv.add_clause s [| lit a |] 0));
    assert_equal Sh.Lfalse (Solv.solve s [| |])

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
    assert_equal Sh.Lfalse (Solv.solve s [| |])

  let generate_php s pigeons holes =
    let module Array = Earray.Array in
    (* phs.(p).(h) tells whether the pigeon p is in the hole h. *)
    let phs =
      Earray.init pigeons
        (fun _ -> Earray.init holes (fun _ -> Solv.new_var s)) in
    (* Each pigeon is in at least one hole. *)
    Earray.iter
      (fun ph ->
        assert_bool "" (Solv.add_clause s (Earray.map lit ph) holes))
      phs;
    (* Each pigeon is in at most one hole. *)
    Earray.iter
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
    assert_equal Sh.Lfalse (Solv.solve s [| |])

  let test_sat () =
    let s = Solv.create () in
    let _ = generate_php s 5 5 in
    assert_equal Sh.Ltrue (Solv.solve s [| |])

  let test_unsat_with_assumpts () =
    let module Array = Earray.Array in
    let s = Solv.create () in
    let phs = generate_php s 4 4  in
    (* No pigeon is in in the second hole. *)
    let assumpts = Earray.map (fun ph -> neg_lit ph.(1)) phs in
    assert_equal Sh.Lfalse (Solv.solve s assumpts);
    assert_equal Sh.Ltrue (Solv.solve s [| |])

  let test_sat_with_assumpts () =
    let module Array = Earray.Array in
    let s = Solv.create () in
    let phs = generate_php s 4 5  in
    (* The first pigeon is in the third hole and the second pigeon
       is in the second hole.
    *)
    let assumpts = [| lit phs.(0).(2); lit phs.(1).(1) |] in
    assert_equal Sh.Ltrue (Solv.solve s assumpts)

  (* Hard problem which cannot be solved in such short time. *)
  let test_interrupt () =
    let solver = of_cnf_file (base_dir ^ "sgen1-unsat-145-100.cnf") in
    let result, interrupted =
      Timer.with_timer 2000
        (fun () -> Solv.interrupt solver)
        (fun () -> Solv.solve solver [| |]) in
    assert_equal Sh.Lundef result;
    assert_bool "" interrupted

  let test_interrupt_sat () =
    let solver = of_cnf_file (base_dir ^ "simple-sat-v3-c2.cnf") in
    let result, interrupted =
      Timer.with_timer (10 * 1000)
        (fun () -> Solv.interrupt solver)
        (fun () -> Solv.solve solver [| |]) in
    assert_equal Sh.Ltrue result;
    assert_bool "" (not interrupted)

  let test_interrupt_unsat () =
    let solver = of_cnf_file (base_dir ^ "simple-unsat-v0-c1.cnf") in
    let result, interrupted =
      Timer.with_timer (10 * 1000)
        (fun () -> Solv.interrupt solver)
        (fun () -> Solv.solve solver [| |]) in
    assert_equal Sh.Lfalse result;
    assert_bool "" (not interrupted)

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
        "interrupt" >:: test_interrupt;
        "interrupt - sat" >:: test_interrupt_sat;
        "interrupt - unsat" >:: test_interrupt_unsat;
      ]

end
