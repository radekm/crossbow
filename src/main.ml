(* Copyright (c) 2013 Radek Micek *)

let (|-) = BatPervasives.(|-)
let (|>) = BatPervasives.(|>)

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let option_to_list = function
  | None -> []
  | Some a -> [a]

type flattening =
  | F_not_preserves
  | F_preserves
  | F_performs

type transform = {
  t_func : 's. 's Prob.t -> unit;
  t_flattening : flattening;
}

let replace_clauses prob clauses =
  BatDynArray.clear prob.Prob.clauses;
  BatDynArray.append clauses prob.Prob.clauses

(* All clauses are transformed at once. *)
let transform_clauses
    prob
    (trans : 's Clause.t BatDynArray.t -> 's Clause.t BatDynArray.t) =

  (* Remove ids. *)
  let cs = BatDynArray.map (fun cl -> cl.Clause2.cl_lits) prob.Prob.clauses in
  let cs' = trans cs in
  (* Add ids. *)
  let cs'' =
    BatDynArray.map
      (fun cl -> { Clause2.cl_id = Prob.fresh_id prob; Clause2.cl_lits = cl })
      cs' in
  replace_clauses prob cs''

(* Each clause is transformed separately. *)
let transform_each_clause prob (trans : 's Clause.t -> 's Clause.t list) =
  transform_clauses
    prob
    (fun cs ->
      let cs' = BatDynArray.create () in
      BatDynArray.iter
        (fun cl -> List.iter (BatDynArray.add cs') (trans cl))
        cs;
      cs')

let simplify_transform =
  let transform prob =
    transform_clauses prob (Clause.simplify_all prob.Prob.symbols) in
  {
    t_func = transform;
    t_flattening = F_preserves;
  }

let rewrite_ground_terms_transform =
  let transform prob =
    transform_clauses prob (Clause.rewrite_ground_terms prob.Prob.symbols) in
  {
    t_func = transform;
    t_flattening = F_preserves;
  }

let unflatten_transform =
  let transform prob =
    transform_each_clause
      prob
      (Clause.unflatten prob.Prob.symbols |- option_to_list) in
  {
    t_func = transform;
    t_flattening = F_not_preserves;
  }

let define_ground_terms_transform =
  let transform prob =
    transform_clauses prob (Term_def.define_ground_terms prob.Prob.symbols) in
  {
    t_func = transform;
    t_flattening = F_not_preserves;
  }

let flatten_transform =
  let transform prob =
    transform_each_clause
      prob
      (Clause.flatten prob.Prob.symbols |- option_to_list) in
  {
    t_func = transform;
    t_flattening = F_performs;
  }

let paradox_splitting_transform =
  let transform prob =
    let db = prob.Prob.symbols in
    transform_each_clause
      prob
      (Splitting.split_clause Splitting.paradox_splitting db) in
  {
    t_func = transform;
    t_flattening = F_preserves;
  }

let paradox_mod_splitting_transform =
  let transform prob =
    let db = prob.Prob.symbols in
    transform_each_clause
      prob
      (Splitting.split_clause Splitting.paradox_mod_splitting db) in
  {
    t_func = transform;
    t_flattening = F_preserves;
  }

type transform_id =
  | T_simplify
  | T_rewrite_ground_terms
  | T_unflatten
  | T_define_ground_terms
  | T_flatten
  | T_paradox_splitting
  | T_paradox_mod_splitting

let all_transforms =
  [
    T_simplify, simplify_transform;
    T_rewrite_ground_terms, rewrite_ground_terms_transform;
    T_unflatten, unflatten_transform;
    T_define_ground_terms, define_ground_terms_transform;
    T_flatten, flatten_transform;
    T_paradox_splitting, paradox_splitting_transform;
    T_paradox_mod_splitting, paradox_mod_splitting_transform;
  ]

let transforms_result_in_flat_clauses =
  List.fold_left
    (fun b tname ->
      match (List.assoc tname all_transforms).t_flattening with
        | F_not_preserves -> false
        | F_preserves -> b
        | F_performs -> true)
    false

type solver =
  | Solv_minisat
  | Solv_cmsat

type only_preproc =
  | Only_preproc_yes
  | Only_preproc_no

(* Transform clauses with ids. *)
let transform_clauses f prob clauses =
  let db = prob.Prob.symbols in
  (* Remove ids. *)
  let cs = BatDynArray.map (fun cl -> cl.Clause2.cl_lits) clauses in
  let cs' = f db cs in
  (* Add ids. *)
  BatDynArray.map
    (fun cl ->
       { Clause2.cl_id = Prob.fresh_id prob; Clause2.cl_lits = cl })
    cs'

let find_model
    only_preproc
    transforms
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
  let transforms =
    match transforms with
      | [] ->
          [
            T_rewrite_ground_terms; T_unflatten; T_define_ground_terms;
            T_flatten; T_paradox_mod_splitting
          ]
      | _ -> transforms in
  if transforms_result_in_flat_clauses transforms |> not then
    failwith "SAT solver needs flat clauses.";
  (* Preprocessing. *)
  List.iter
    (fun tname ->
      let t = List.assoc tname all_transforms in
      t.t_func p)
    transforms;
  (* Normalize variables. *)
  transform_each_clause p (fun cl -> [Clause.normalize_vars cl |> fst]);
  let with_output f =
    match output_file with
      | None -> f BatPervasives.stdout
      | Some file -> BatFile.with_file_out file f in
  if only_preproc = Only_preproc_yes then begin
    let b = Buffer.create 1024 in
    with_output
      (fun out ->
        Tptp_prob.prob_to_tptp tptp_prob true
          (fun f ->
            Tptp.write b f;
            BatBuffer.output_buffer out b;
            Buffer.clear b))
  end else begin
    Printf.fprintf stderr "Clauses: %d\n" (BatDynArray.length p.Prob.clauses);
    let sorts = Sorts.of_problem p in
    let inst = Solver.create p sorts in
    (* Model search. *)
    let found = ref false in
    let interrupted = ref false in
    let dsize = ref 0 in
    while not !found && not !interrupted && has_time () do
      incr dsize;
      Printf.fprintf stderr "Instantiating - domain size %d (%d ms)\n"
        !dsize (Timer.get_ms () - start_ms);
      flush stderr;
      Solver.incr_max_size inst;
      Printf.fprintf stderr "Solving (%d ms)\n" (Timer.get_ms () - start_ms);
      flush stderr;
      match remaining_ms () with
        | None ->
            begin match Solver.solve inst with
              | Sh.Ltrue -> found := true
              | Sh.Lfalse -> ()
              | Sh.Lundef ->
                  failwith "unexpected result from SAT solver"
            end
        | Some ms ->
            begin match Solver.solve_timed inst ms with
              | _, true -> interrupted := true
              | Sh.Ltrue, _ -> found := true
              | Sh.Lfalse, _ -> ()
              | Sh.Lundef, _ ->
                  failwith "unexpected result from SAT solver"
            end
    done;
    Printf.fprintf stderr "Stop (%d ms)\n" (Timer.get_ms () - start_ms);
    flush stderr;
    if !found then begin
      Printf.fprintf stderr "Constructing multi-sorted model\n";
      flush stderr;
      let ms_model = Solver.construct_model inst in
      Printf.fprintf stderr "Constructing model with single sort\n";
      flush stderr;
      let model = Model.of_ms_model ms_model sorts in
      let b = Buffer.create 1024 in
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

let transforms =
  let flags = [
    T_simplify,
    Arg.info ["simplify"] ~docs:"TRANSFORMATIONS";
    T_rewrite_ground_terms,
    Arg.info ["rewrite-ground-terms"] ~docs:"TRANSFORMATIONS";
    T_unflatten,
    Arg.info ["unflatten"] ~docs:"TRANSFORMATIONS";
    T_define_ground_terms,
    Arg.info ["define-ground-terms"] ~docs:"TRANSFORMATIONS";
    T_flatten,
    Arg.info ["flatten"] ~docs:"TRANSFORMATIONS";
    T_paradox_splitting,
    Arg.info ["paradox-splitting"] ~docs:"TRANSFORMATIONS";
    T_paradox_mod_splitting,
    Arg.info ["paradox-mod-splitting"] ~docs:"TRANSFORMATIONS";
  ] in
  Arg.(value & vflag_all [] flags)

let only_preproc =
  let doc = "$(docv) can be: yes, no." in
  let values = [
    "yes", Only_preproc_yes;
    "no", Only_preproc_no;
  ] in
  Arg.(value & opt (enum values) Only_preproc_no &
         info ["only-preproc"] ~docv:"ONLY-PREPROC" ~doc)

let find_model_t =
  Term.(pure find_model $ only_preproc $ transforms $
          solver $ max_secs $
          output_file $ base_dir $ in_file)

let info =
  let doc = "finite model finder" in
  Term.info "crossbow" ~version:"0.1" ~doc

let () =
  match Term.eval (find_model_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
