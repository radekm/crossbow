(* Copyright (c) 2013 Radek Micek *)

let (|-) = BatPervasives.(|-)
let (|>) = BatPervasives.(|>)

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let option_to_list = function
  | None -> []
  | Some a -> [a]

(* ************************************************************************ *)
(* Transforms *)

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

(* ************************************************************************ *)
(* Solvers *)

type solver_config = {
  nthreads : int;
  all_models : bool;
  n_from : int;
  n_to : int;
  output_file : string option;
  start_ms : int;
  max_ms : int option;
}

let with_output ?(append = false) cfg f =
  match cfg.output_file with
    | None -> f BatPervasives.stdout; BatIO.flush BatPervasives.stdout
    | Some file ->
        let mode = if append then [ `append] else [] in
        BatFile.with_file_out ~mode file (fun out -> f out; BatIO.flush out)

let remaining_ms cfg =
  match cfg.max_ms with
    | None -> None
    | Some max_ms -> Some (cfg.start_ms + max_ms - Timer.get_ms ())

let has_time cfg =
  match remaining_ms cfg with
    | None -> true
    | Some ms -> ms > 0

type solver = {
  s_func : 's. 's Tptp_prob.t -> solver_config -> unit;
  s_only_flat_clauses : bool;
  s_default_transforms : transform_id list;
}

let write_model tp model number out =
  let b = Buffer.create 1024 in
  let formula_name =
    match number with
      | None -> "interp"
      | Some n -> "interp" ^ string_of_int n in
  Tptp_prob.model_to_tptp tp model
    (Tptp_ast.N_word (Tptp_ast.to_plain_word formula_name))
    (fun f ->
      Tptp.write b f;
      BatBuffer.output_buffer out b;
      Buffer.clear b)

let print_with_time cfg str =
  Printf.fprintf stderr "%s (%d ms)\n" str (Timer.get_ms () - cfg.start_ms);
  flush stderr

let call_solver cfg inst solve solve_timed =
  match remaining_ms cfg with
    | None ->
        begin match solve inst with
          | Sh.Ltrue -> Sh.Ltrue
          | Sh.Lfalse -> Sh.Lfalse
          | Sh.Lundef ->
              failwith "unexpected result from the solver"
        end
    | Some ms ->
        begin match solve_timed inst ms with
          | _, true -> Sh.Lundef
          | Sh.Ltrue, _ -> Sh.Ltrue
          | Sh.Lfalse, _ -> Sh.Lfalse
          | Sh.Lundef, _ ->
              failwith "unexpected result from the solver"
        end

let sat_solve (module Inst : Sat_inst.Inst_sig) tp cfg =
  let print_instantiating dsize =
    print_with_time cfg (Printf.sprintf "Instantiating %d" dsize) in

  let p = tp.Tptp_prob.prob in
  let sorts = Sorts.of_problem p in
  let inst = Inst.create p sorts in
  let model_cnt = ref 0 in

  Printf.fprintf stderr "Clauses: %d\n" (BatDynArray.length p.Prob.clauses);

  for dsize = 1 to cfg.n_from - 1 do
    print_instantiating dsize;
    Inst.incr_max_size inst
  done;

  if cfg.all_models then begin
    let dsize = cfg.n_from in
    print_instantiating dsize;
    Inst.incr_max_size inst;
    if dsize < BatDynArray.length p.Prob.distinct_consts then
      Printf.fprintf stderr "\n"
    else begin
      let tot_ms_model_cnt = ref 0 in
      let ms_models = ref (BatSet.create Ms_model.compare) in
      let rec loop () =
        if not (has_time cfg) then
          print_with_time cfg "\nTime out"
        else begin
          match call_solver cfg inst Inst.solve Inst.solve_timed with
            | Sh.Ltrue ->
                incr tot_ms_model_cnt;
                let ms_model = Inst.construct_model inst in
                Inst.block_model inst ms_model;
                let cano_ms_model = Ms_model.canonize ms_model sorts in
                if not (BatSet.mem cano_ms_model !ms_models) then begin
                  ms_models := BatSet.add cano_ms_model !ms_models;
                  let models = Model.all_of_ms_model cano_ms_model sorts in
                  BatSet.iter
                    (fun model ->
                      with_output
                        ~append:true
                        cfg
                        (write_model tp model (Some !model_cnt));
                      incr model_cnt)
                    models
                end;
                loop ()
            | Sh.Lfalse -> Printf.fprintf stderr "\n"
            | Sh.Lundef -> print_with_time cfg "\nTime out"
        end in
      print_with_time cfg "Solving";
      with_output cfg (fun _ -> ()); (* Truncate file. *)
      loop ();
      Printf.fprintf stderr "%d multi-sorted models found\n" !tot_ms_model_cnt
    end
  end else begin
    let rec loop dsize =
      if dsize > cfg.n_to then
        Printf.fprintf stderr "\n"
      else if not (has_time cfg) then
        print_with_time cfg "\nTime out"
      else begin
        print_instantiating dsize;
        Inst.incr_max_size inst;
        let result =
          if dsize < BatDynArray.length p.Prob.distinct_consts then
            Sh.Lfalse
          else
            let _ = print_with_time cfg "Solving" in
            call_solver cfg inst Inst.solve Inst.solve_timed in
        match result with
          | Sh.Ltrue ->
              let ms_model = Inst.construct_model inst in
              let model = Model.of_ms_model ms_model sorts in
              with_output cfg (write_model tp model None);
              incr model_cnt;
              Printf.fprintf stderr "\n"
          | Sh.Lfalse -> loop (dsize + 1)
          | Sh.Lundef -> print_with_time cfg "\nTime out"
      end in
    loop cfg.n_from
  end;

  match !model_cnt with
    | 0 -> print_with_time cfg "No model found"
    | 1 -> print_with_time cfg "1 model found"
    | n -> print_with_time cfg (Printf.sprintf "%d non-isomorphic models found" n)

let csp_solve (module Inst : Csp_inst.Inst_sig) tp cfg =
  let print_instantiating dsize =
    print_with_time cfg (Printf.sprintf "Instantiating %d" dsize) in

  let p = tp.Tptp_prob.prob in
  let model_cnt = ref 0 in

  Printf.fprintf stderr "Clauses: %d\n" (BatDynArray.length p.Prob.clauses);

  if cfg.all_models then begin
    let dsize = cfg.n_from in
    if dsize < BatDynArray.length p.Prob.distinct_consts then
      Printf.fprintf stderr "\n"
    else begin
      print_instantiating dsize;
      let inst = Inst.create ~nthreads:cfg.nthreads p dsize in
      let tot_model_cnt = ref 0 in
      let models = ref (BatSet.create Model.compare) in
      let rec loop () =
        if not (has_time cfg) then
          print_with_time cfg "\nTime out"
        else begin
          match call_solver cfg inst Inst.solve Inst.solve_timed with
            | Sh.Ltrue ->
                incr tot_model_cnt;
                let model = Inst.construct_model inst in
                let cano_model = Model.canonize model in
                if not (BatSet.mem cano_model !models) then begin
                  models := BatSet.add cano_model !models;
                  with_output
                    ~append:true
                    cfg
                    (write_model tp model (Some !model_cnt));
                  incr model_cnt;
                end;
                loop ()
            | Sh.Lfalse -> Printf.fprintf stderr "\n"
            | Sh.Lundef -> print_with_time cfg "\nTime out"
        end in
      print_with_time cfg "Solving";
      with_output cfg (fun _ -> ()); (* Truncate file. *)
      loop ();
      Printf.fprintf stderr "%d models found\n" !tot_model_cnt
    end
  end else begin
    let rec loop dsize =
      if dsize > cfg.n_to then
        Printf.fprintf stderr "\n"
      else if not (has_time cfg) then
        print_with_time cfg "\nTime out"
      else if dsize < BatDynArray.length p.Prob.distinct_consts then
        loop (dsize + 1)
      else begin
        print_instantiating dsize;
        let inst =
          Inst.create  ~nthreads:cfg.nthreads p dsize in
        print_with_time cfg "Solving";
        match call_solver cfg inst Inst.solve Inst.solve_timed with
          | Sh.Ltrue ->
              let model = Inst.construct_model inst in
              with_output cfg (write_model tp model None);
              incr model_cnt;
              Printf.fprintf stderr "\n"
          | Sh.Lfalse -> loop (dsize + 1)
          | Sh.Lundef -> print_with_time cfg "\nTime out"
      end in
    loop cfg.n_from
  end;

  match !model_cnt with
    | 0 -> print_with_time cfg "No model found"
    | 1 -> print_with_time cfg "1 model found"
    | n -> print_with_time cfg (Printf.sprintf "%d non-isomorphic models found" n)

let minisat_solver =
  let s_func tp cfg =
    sat_solve (module Minisat_inst.Inst : Sat_inst.Inst_sig) tp cfg in
  {
    s_func;
    s_only_flat_clauses = true;
    s_default_transforms = [
      T_rewrite_ground_terms; T_unflatten; T_define_ground_terms;
      T_flatten; T_paradox_mod_splitting
    ];
  }

let cmsat_solver =
  let s_func tp cfg =
    sat_solve (module Cmsat_inst.Inst : Sat_inst.Inst_sig) tp cfg in
  {
    s_func;
    s_only_flat_clauses = true;
    s_default_transforms = [
      T_rewrite_ground_terms; T_unflatten; T_define_ground_terms;
      T_flatten; T_paradox_mod_splitting
    ];
  }

let gecode_solver =
  let s_func tp cfg =
    csp_solve (module Gecode_inst.Inst : Csp_inst.Inst_sig) tp cfg in
  {
    s_func;
    s_only_flat_clauses = false;
    s_default_transforms = [ T_rewrite_ground_terms; T_unflatten ];
  }

let only_preproc_solver =
  let s_func tp cfg =
    let b = Buffer.create 1024 in
    with_output cfg
      (fun out ->
        Tptp_prob.prob_to_tptp tp true
          (fun f ->
            Tptp.write b f;
            BatBuffer.output_buffer out b;
            Buffer.clear b)) in
  {
    s_func;
    s_only_flat_clauses = false;
    s_default_transforms = [];
  }

type solver_id =
  | Solv_minisat
  | Solv_cmsat
  | Solv_gecode
  | Solv_only_preproc

let all_solvers =
  [
    Solv_minisat, minisat_solver;
    Solv_cmsat, cmsat_solver;
    Solv_gecode, gecode_solver;
    Solv_only_preproc, only_preproc_solver;
  ]

let find_model
    transforms
    solver
    n_from
    n_to
    all_models
    max_secs
    output_file
    base_dir
    in_file =

  let start_ms = Timer.get_ms () in
  let n_to = BatOption.default max_int n_to in
  if n_from < 1 || n_to < 1 then
    failwith "Minimal domain size is 1.";
  let Tptp_prob.Wr tptp_prob = Tptp_prob.of_file base_dir in_file in
  let p = tptp_prob.Tptp_prob.prob in
  let solver = List.assoc solver all_solvers in
  let transforms =
    match transforms with
      | [] -> solver.s_default_transforms
      | _ -> transforms in
  if
    solver.s_only_flat_clauses &&
    not (transforms_result_in_flat_clauses transforms)
  then
    failwith "Solver needs flat clauses.";
  (* Preprocessing. *)
  List.iter
    (fun tname ->
      let t = List.assoc tname all_transforms in
      t.t_func p)
    transforms;
  (* Normalize variables. *)
  transform_each_clause p (fun cl -> [Clause.normalize_vars cl |> fst]);
  (* Run selected solver. *)
  let cfg = {
    nthreads = 1;
    all_models;
    n_from;
    n_to;
    output_file;
    start_ms;
    max_ms = BatOption.map (fun secs -> secs * 1000) max_secs;
  } in
  solver.s_func tptp_prob cfg

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

let n_from =
  let doc = "Start search with domain size $(docv)." in
  Arg.(value & opt int 1 &
         info ["from"] ~docv:"N" ~doc)

let n_to =
  let doc = "Stop search with domain size $(docv)." in
  Arg.(value & opt (some int) None &
         info ["to"] ~docv:"N" ~doc)

let all_models =
  let doc = "Find all models." in
  Arg.(value & flag & info ["all-models"] ~doc)

let solver =
  let doc = "$(docv) can be: cryptominisat, minisat, gecode, only-preproc." in
  let values = [
    "cryptominisat", Solv_cmsat;
    "minisat", Solv_minisat;
    "gecode", Solv_gecode;
    "only-preproc", Solv_only_preproc;
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

let find_model_t =
  Term.(pure find_model $ transforms $ solver $
          n_from $ n_to $ all_models $
          max_secs $ output_file $ base_dir $ in_file)

let info =
  let doc = "finite model finder" in
  Term.info "crossbow" ~version:"0.1" ~doc

let () =
  match Term.eval (find_model_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
