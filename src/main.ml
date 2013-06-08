(* Copyright (c) 2013 Radek Micek *)

let (|-) = BatPervasives.(|-)

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

type solver =
  | Solv_minisat
  | Solv_cmsat

type splitting =
  | Spl_none
  | Spl_paradox
  | Spl_paradox_mod

type unflatten =
  | Unfl_no
  | Unfl_yes

let find_model
    unflatten
    splitting
    solver
    max_secs
    output_file
    base_dir
    in_file =

  let start_ms = Timer.get_ms () in
  let remaining_ms () =
    match max_secs with
      | None -> None
      | Some max_secs ->
          Some (start_ms + max_secs * 1000 - Timer.get_ms ()) in
  let has_time () =
    match remaining_ms () with
      | None -> true
      | Some ms -> ms > 0 in
  let solvers = [
    Solv_minisat, (module Minisat_inst.Inst : Sat_inst.Inst_sig);
    Solv_cmsat, (module Cmsat_inst.Inst : Sat_inst.Inst_sig);
  ] in
  let module Solver = (val List.assoc solver solvers) in
  let Tptp_prob.Wr tptp_prob = Tptp_prob.of_file base_dir in_file in
  let p = tptp_prob.Tptp_prob.prob in
  let symb_db = p.Prob.symbols in
  (* Preprocessing. *)
  let orig_clauses = BatDynArray.copy p.Prob.clauses in
  let unflat_clauses =
    match unflatten with
      | Unfl_no -> orig_clauses
      | Unfl_yes ->
          BatDynArray.filter_map
            (fun cl ->
              match Clause.unflatten symb_db cl.Clause2.cl_lits with
                | None -> None
                | Some cl_lits -> Some { cl with Clause2.cl_lits })
            orig_clauses in
  let flat_clauses =
    BatDynArray.filter_map
      (fun cl ->
        match Clause.flatten symb_db cl.Clause2.cl_lits with
          | None -> None
          | Some cl_lits -> Some { cl with Clause2.cl_lits })
      unflat_clauses in
  let splitted_clauses =
    match splitting with
      | Spl_none -> flat_clauses
      | _ ->
          let strategy =
            List.assoc splitting
              [
                Spl_paradox, Splitting.paradox_splitting;
                Spl_paradox_mod, Splitting.paradox_mod_splitting;
              ] in
          let cs = BatDynArray.make (BatDynArray.length flat_clauses) in
          BatDynArray.iter
            (fun cl ->
              List.iter
                (fun cl_lits ->
                  BatDynArray.add cs
                    { Clause2.cl_id = Prob.fresh_id p; Clause2.cl_lits })
                (Splitting.split_clause strategy symb_db cl.Clause2.cl_lits))
            flat_clauses;
          cs in
  BatDynArray.clear p.Prob.clauses;
  BatDynArray.append splitted_clauses p.Prob.clauses;
  Printf.fprintf stderr "Clauses: %d\n" (BatDynArray.length p.Prob.clauses);
  let sorts = Sorts.of_problem p in
  let inst = Solver.create p sorts in
  (* Model search. *)
  let found = ref false in
  let interrupted = ref false in
  let dsize = ref 0 in
  while not !found && not !interrupted && has_time () do
    incr dsize;
    Printf.fprintf stderr "Instantiating - domain size %d\n" !dsize;
    flush stderr;
    Solver.incr_max_size inst;
    Printf.fprintf stderr "Solving\n";
    flush stderr;
    match remaining_ms () with
      | None ->
          begin match Solver.solve inst with
            | Sat_solver.Ltrue -> found := true
            | Sat_solver.Lfalse -> ()
            | Sat_solver.Lundef ->
                failwith "unexpected result from SAT solver"
          end
      | Some ms ->
          begin match Solver.solve_timed inst ms with
            | _, true -> interrupted := true
            | Sat_solver.Ltrue, _ -> found := true
            | Sat_solver.Lfalse, _ -> ()
            | Sat_solver.Lundef, _ ->
                failwith "unexpected result from SAT solver"
          end
  done;
  if !found then begin
    Printf.fprintf stderr "Constructing multi-sorted model\n";
    flush stderr;
    let ms_model = Solver.construct_model inst in
    Printf.fprintf stderr "Constructing model with single sort\n";
    flush stderr;
    let model = Model.of_ms_model ms_model sorts in
    let b = Buffer.create 1024 in
    let with_output f =
      match output_file with
        | None -> f BatPervasives.stdout
        | Some file -> BatFile.with_file_out file f in
    with_output
      (fun out ->
        Tptp_prob.model_to_tptp tptp_prob model
          (Tptp_ast.N_word (Tptp_ast.to_plain_word "interp"))
          (fun f ->
            Tptp.write b f;
            BatBuffer.output_buffer out b;
            Buffer.clear b))
  end else begin
    Printf.fprintf stderr "Time out\n";
    flush stderr;
  end

let in_file =
  let doc = "File with CNF clauses in TPTP format." in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"INPUT" ~doc)

let base_dir =
  let doc = "Relative include paths are resolved against this directory." in
  Arg.(value & opt dir "." & info ["base-dir"] ~docv:"DIR" ~doc)

let output_file =
  let doc = "Write model to this file." in
  Arg.(value & opt (some string) None &
         info ["output-file"] ~docv:"FILE" ~doc)

let max_secs =
  let doc = "Stop search after $(docv) seconds." in
  Arg.(value & opt (some int) None &
         info ["max-secs"] ~docv:"N" ~doc)

let solver =
  let doc = "$(docv) can be: cryptominisat, minisat." in
  let values = [
    "cryptominisat", Solv_cmsat;
    "minisat", Solv_minisat;
  ] in
  Arg.(value & opt (enum values) Solv_cmsat &
         info ["solver"] ~docv:"SOLVER" ~doc)

let splitting =
  let doc = "$(docv) can be: paradox-mod, paradox, none." in
  let values = [
    "paradox-mod", Spl_paradox_mod;
    "paradox", Spl_paradox;
    "none", Spl_none;
  ] in
  Arg.(value & opt (enum values) Spl_paradox_mod &
         info ["splitting"] ~docv:"SPLITTING" ~doc)

let unflatten =
  let doc = "$(docv) can be: yes, no." in
  let values = [
    "yes", Unfl_yes;
    "no", Unfl_no;
  ] in
  Arg.(value & opt (enum values) Unfl_yes &
         info ["unflatten"] ~docv:"UNFLATTEN" ~doc)

let find_model_t =
  Term.(pure find_model $ unflatten $ splitting $ solver $
          max_secs $ output_file $ base_dir $ in_file)

let info =
  let doc = "finite model finder" in
  Term.info "crossbow" ~version:"0.1" ~doc

let () =
  match Term.eval (find_model_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
