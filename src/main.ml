(* Copyright (c) 2013 Radek Micek *)

let (|-) = BatPervasives.(|-)

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let find_model base_dir in_file =
  let tptp_prob = Tptp_prob.of_file base_dir in_file in
  let p = tptp_prob.Tptp_prob.prob in
  let symb_db = p.Prob.symbols in
  (* Preprocessing. *)
  let flat_clauses =
    BatDynArray.filter_map
      (Clause.unflatten symb_db |- BatOption.bind (Clause.flatten symb_db))
      p.Prob.clauses in
  let splitted_clauses =
    let cs = BatDynArray.make (BatDynArray.length flat_clauses) in
    BatDynArray.iter
      (fun cl ->
        List.iter
          (BatDynArray.add cs)
          (Splitting.split_clause Splitting.paradox_mod_splitting p cl))
      flat_clauses;
    cs in
  BatDynArray.clear p.Prob.clauses;
  BatDynArray.append splitted_clauses p.Prob.clauses;
  Printf.fprintf stderr "Clauses: %d\n" (BatDynArray.length p.Prob.clauses);
  let sorts = Sorts.of_problem p in
  let inst = Minisat_inst.Inst.create p sorts in
  (* Model search. *)
  let found = ref false in
  let dsize = ref 0 in
  while not !found do
    incr dsize;
    Printf.fprintf stderr "Instantiating - domain size %d\n" !dsize;
    flush stderr;
    Minisat_inst.Inst.incr_max_size inst;
    Printf.fprintf stderr "Solving\n";
    flush stderr;
    match Minisat_inst.Inst.solve inst with
      | Sat_solver.Ltrue -> found := true
      | Sat_solver.Lfalse -> ()
      | Sat_solver.Lundef ->
          failwith "unexpected result from SAT solver"
  done;
  Printf.fprintf stderr "Constructing multi-sorted model\n";
  flush stderr;
  let ms_model = Minisat_inst.Inst.construct_model inst in
  Printf.fprintf stderr "Constructing model with single sort\n";
  flush stderr;
  let model = Model.of_ms_model ms_model sorts in
  let b = Buffer.create 1024 in
  Tptp_prob.model_to_tptp tptp_prob model
    (Tptp_ast.N_word (Tptp_ast.to_plain_word "interp"))
    (fun f ->
      Tptp.write b f;
      Buffer.output_buffer stdout b;
      Buffer.clear b)

let in_file =
  let doc = "File with CNF clauses in TPTP format." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"INPUT" ~doc)

let base_dir =
  let doc = "Relative include paths are resolved against this directory." in
  Arg.(value & opt dir "." & info ["base-dir"] ~docv:"DIR" ~doc)

let find_model_t = Term.(pure find_model $ base_dir $ in_file)

let info =
  let doc = "finite model finder" in
  Term.info "crossbow" ~version:"0.1" ~doc

let () =
  match Term.eval (find_model_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
