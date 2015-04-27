(* Copyright (c) 2013-15 Radek Micek *)

let (%>) = BatPervasives.(%>)

module Array = Earray.Array
module T = Term
module L = Lit

module Path = BatPathGen.OfString

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let option_to_list = function
  | None -> []
  | Some a -> [a]

let file_in_program_dir file =
  Path.of_string Sys.executable_name
  |> Path.map_name (fun _ -> file)
  |> Path.to_ustring

let preds_in_clause lits =
  let preds = ref BatSet.empty in
  List.iter
    (fun (L.Lit (_, s, _)) ->
      if s <> Symb.sym_eq then
        preds := BatSet.add s !preds)
    lits;
  !preds

let funcs_in_clause lits =
  let funcs = ref BatSet.empty in
  List.iter
    (fun lit ->
      L.iter
        (function
        | T.Var _ -> ()
        | T.Func (s, _) -> funcs := BatSet.add s !funcs)
        lit)
    lits;
  !funcs

let contains_empty_clause p =
  try
    BatDynArray.iter
      (fun cl -> if cl.Clause2.cl_lits = [] then raise Exit)
      p.Prob.clauses;
    false
  with
    | Exit -> true

(* ************************************************************************ *)
(* Printing *)

(* Note: Doesn't print symbol properties (eg. commutativity)
   discovered by lemma generation.
*)
let print_lemmas verbose tp lemmas =
  Printf.fprintf stderr "%d lemma(s) from E\n"
    (BatDynArray.length lemmas);

  if verbose then
    BatDynArray.iter
      (fun cl ->
        Printf.fprintf stderr "    %s"
          (Tptp_prob.clause_to_string
             tp.Tptp_prob.prob.Prob.symbols
             tp.Tptp_prob.smap
             cl.Clause2.cl_lits))
      lemmas

let print_symb_info verbose tp =
  let p = tp.Tptp_prob.prob in
  let symdb = p.Prob.symbols in
  let clauses =
    p.Prob.clauses
    |> Earray.of_dyn_array
    |> Earray.map (fun cl -> cl.Clause2.cl_lits) in
  let preds =
    Earray.fold_left
      (fun acc -> preds_in_clause %> BatSet.union acc)
      BatSet.empty
      clauses in
  let funcs =
    Earray.fold_left
      (fun acc -> funcs_in_clause %> BatSet.union acc)
      BatSet.empty
      clauses in
  Printf.fprintf stderr "%d pred(s)\n" (BatSet.cardinal preds);
  if verbose then
    BatSet.iter
      (fun s ->
        let symmetric =
          if Symb.commutative symdb s
          then ", symmetric"
          else "" in
        Printf.fprintf stderr "    %3d arity%s\n" (Symb.arity s) symmetric)
      preds;
  Printf.fprintf stderr "%d func(s)\n" (BatSet.cardinal funcs);
  if verbose then
    BatSet.iter
      (fun s ->
        let commutative =
          if Symb.commutative symdb s
          then ", commutative"
          else "" in
        let hint =
          match Symb.hints symdb s with
            | [] -> ""
            | [Symb.Permutation] -> ", permutation"
            | [Symb.Latin_square] -> ", latin square"
            | _ :: _ -> failwith "hints" in
        Printf.fprintf stderr "    %3d arity%s%s\n"
          (Symb.arity s) commutative hint)
      funcs

let print_clause_info verbose tp =
  let p = tp.Tptp_prob.prob in
  let clauses = p.Prob.clauses in
  Printf.fprintf stderr "%d clause(s)\n" (BatDynArray.length clauses);
  if verbose then begin
    let compute_stats cl =
      let cl = cl.Clause2.cl_lits in
      let _, nvars = Clause.normalize_vars cl in
      let nlits = List.length cl in
      let npreds = preds_in_clause cl |> BatSet.cardinal in
      let nfuncs = funcs_in_clause cl |> BatSet.cardinal in
      (nvars, nlits, npreds, nfuncs) in
    let stats =
      clauses
      |> Earray.of_dyn_array
      |> Earray.map compute_stats in
    Earray.iter
      (fun (nvars, nlits, npreds, nfuncs) ->
        Printf.fprintf stderr
          "    %3d var(s), %3d lit(s), %3d pred(s), %3d func(s)\n"
          nvars nlits npreds nfuncs)
      stats
  end

let print_sort_info verbose sorts =
  let nsorts = Earray.length sorts.Sorts.adeq_sizes in
  Printf.fprintf stderr "%d sort(s)\n" nsorts;
  if verbose then
    for sort = 0 to nsorts - 1 do
      Printf.fprintf stderr
        "    %3d adequate size, %3d const(s)\n"
        sorts.Sorts.adeq_sizes.(sort)
        (Earray.length sorts.Sorts.consts.(sort))
    done

(* ************************************************************************ *)
(* Transforms *)

type flattening =
  | F_not_preserves
  | F_preserves
  | F_performs

type transform = {
  t_func : [`R|`W] Prob.t -> unit;
  t_flattening : flattening;
}

let replace_clauses prob clauses =
  BatDynArray.clear prob.Prob.clauses;
  BatDynArray.append clauses prob.Prob.clauses

(* All clauses are transformed at once. *)
let transform_clauses
    prob
    (trans : Clause.t BatDynArray.t -> Clause.t BatDynArray.t) =

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
let transform_each_clause prob (trans : Clause.t -> Clause.t list) =
  transform_clauses
    prob
    (fun cs ->
      let cs' = BatDynArray.create () in
      BatDynArray.iter
        (fun cl -> List.iter (BatDynArray.add cs') (trans cl))
        cs;
      cs')

let detect_commutativity_transform =
  let transform prob =
    transform_clauses
      prob
      (Prop_det.detect_commutativity prob.Prob.symbols) in
  {
    t_func = transform;
    t_flattening = F_preserves;
  }

let detect_hints_for_groups_transform =
  let transform prob =
    transform_clauses
      prob
      (fun cs ->
        Prop_det.detect_hints_for_groups prob.Prob.symbols cs;
        cs) in
  {
    t_func = transform;
    t_flattening = F_preserves;
  }

let detect_hints_for_quasigroups_transform =
  let transform prob =
    transform_clauses
      prob
      (fun cs ->
        Prop_det.detect_hints_for_quasigroups prob.Prob.symbols cs;
        cs) in
  {
    t_func = transform;
    t_flattening = F_preserves;
  }

let detect_hints_for_involutive_funcs_transform =
  let transform prob =
    transform_clauses
      prob
      (fun cs ->
        Prop_det.detect_hints_for_involutive_funcs prob.Prob.symbols cs;
        cs) in
  {
    t_func = transform;
    t_flattening = F_preserves;
  }

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
      (Clause.unflatten prob.Prob.symbols %> option_to_list) in
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
      (Clause.flatten prob.Prob.symbols %> option_to_list) in
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
  | T_detect_commutativity
  | T_detect_hints_for_groups
  | T_detect_hints_for_quasigroups
  | T_detect_hints_for_involutive_funcs
  | T_simplify
  | T_rewrite_ground_terms
  | T_unflatten
  | T_define_ground_terms
  | T_flatten
  | T_paradox_splitting
  | T_paradox_mod_splitting

let all_transforms =
  [
    T_detect_commutativity, detect_commutativity_transform;
    T_detect_hints_for_groups, detect_hints_for_groups_transform;
    T_detect_hints_for_quasigroups, detect_hints_for_quasigroups_transform;
    T_detect_hints_for_involutive_funcs,
      detect_hints_for_involutive_funcs_transform;
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
(* Lemma generation *)

let clause_weight lits =
  List.length lits +
  BatSet.cardinal (preds_in_clause lits) +
  BatSet.cardinal (funcs_in_clause lits) +
  Sh.IntSet.cardinal (Clause.vars lits)

let compare_clauses a b = clause_weight a - clause_weight b

(* [smap] will be extended to contain all symbols from the clauses given
   to the lemma generator. On the other hand symbols introduced
   by the lemma generator will be added to [symdb]
   and won't be added to [smap].
*)
let generate_lemmas_by_e
    tp
    e_exe
    e_max_secs
    e_opts
    detect_commutativity
    max_vars
    max_symbs
    max_vars_when_flat
    max_lits_when_flat
    max_lemmas =

  let symdb = tp.Tptp_prob.prob.Prob.symbols in

  (* Convert problem to TPTP. *)
  let inputs = BatDynArray.create () in
  let seen_symbs =
    Tptp_prob.prob_to_tptp tp Tptp_prob.Export (BatDynArray.add inputs) in

  (* Use E to generate lemmas. *)
  let lemmas =
    Eprover.generate_lemmas e_exe e_opts e_max_secs
      (BatDynArray.to_list inputs) in

  (* Read generated lemmas from TPTP. *)
  let lemmas =
    (* Using the restricted [smap] ensures that symbols introduced
       by the lemma generator are treated as new symbols even if their names
       coincide with the names from the original [smap].
    *)
    let smap = Tptp_prob.restrict_symb_map seen_symbs tp.Tptp_prob.smap in
    lemmas
    |> Tptp_prob.clauses_of_tptp symdb smap
    |> BatDynArray.of_list in

  (* Detect commutativity. *)
  let lemmas =
    if detect_commutativity
    then Prop_det.detect_commutativity symdb lemmas
    else lemmas in

  (* Unflatten. *)
  let lemmas = BatDynArray.filter_map (Clause.unflatten symdb) lemmas in

  (* Filter clauses from E prover by [max_vars], [max_symbs]
     [max_vars_when_flat] and [max_lits_when_flat].
  *)
  let get_lits cl = cl.Clause2.cl_lits in
  let orig_clauses =
    tp.Tptp_prob.prob.Prob.clauses
    |> BatDynArray.to_array
    |> BatArray.filter_map (get_lits %> Clause.unflatten symdb) in
  BatDynArray.keep
    (fun cl ->
      let cl, nvars = Clause.normalize_vars cl in
      let nsymbs =
        BatSet.cardinal (preds_in_clause cl) +
        BatSet.cardinal (funcs_in_clause cl) in
      if nvars <= max_vars && nsymbs <= max_symbs then begin
        match Clause.flatten symdb cl with
          | Some fcl ->
              let _, fnvars = Clause.normalize_vars fcl in
              fnvars <= max_vars_when_flat &&
              List.length fcl <= max_lits_when_flat &&
              (* Skip original clauses. *)
              BatArray.for_all ((<>) cl) orig_clauses
          | None -> false
      end else
        false)
    lemmas;

  (* Keep at most [max_lemmas] with the smallest weights. *)
  let lemmas =
    lemmas
    |> BatDynArray.to_list
    |> List.sort compare_clauses
    |> BatList.take max_lemmas
    |> BatDynArray.of_list in

  BatDynArray.map
    (fun lits ->
      {
        Clause2.cl_id = Prob.fresh_id tp.Tptp_prob.prob;
        Clause2.cl_lits = lits
      })
    lemmas

(* ************************************************************************ *)
(* Solvers *)

type solver_config = {
  nthreads : int;
  all_models : bool;
  n_from : int;
  n_to : int;
  in_file : string;
  has_conjecture : bool;
  output_file : string option;
  start_ms : int;
  max_ms : int option;
}

let with_output ?(append = false) cfg f =
  match cfg.output_file with
    | None -> f BatPervasives.stdout; BatIO.flush BatPervasives.stdout
    | Some file ->
        let mode = if append then [`append] else [`create] in
        BatFile.with_file_out ~mode file (fun out -> f out; BatIO.flush out)

type summary =
  | S_satisfiable
  | S_unsatisfiable
  | S_timeout
  | S_gave_up

let write_summary cfg summary =
  let s =
    match summary with
      | S_satisfiable ->
          if cfg.has_conjecture
          then "CounterSatisfiable"
          else "Satisfiable"
      | S_unsatisfiable ->
          if cfg.has_conjecture
          then "Theorem"
          else "Unsatisfiable"
      | S_timeout -> "Timeout"
      | S_gave_up -> "GaveUp" in
  let problem = Filename.basename cfg.in_file in
  with_output cfg
    (fun out ->
      BatPrintf.fprintf out "%% SZS status %s for %s\n\n" s problem)

let remaining_ms cfg =
  match cfg.max_ms with
    | None -> None
    | Some max_ms -> Some (cfg.start_ms + max_ms - Timer.get_ms ())

let has_time cfg =
  match remaining_ms cfg with
    | None -> true
    | Some ms -> ms > 0

type solver = {
  s_func : Tptp_prob.t -> Sorts.t -> solver_config -> unit;
  s_only_flat_clauses : bool;
  s_default_transforms : transform_id list;
}

let write_model in_file tp model number out =
  let b = Buffer.create 1024 in
  let prob_name = Filename.basename in_file in
  let formula_name =
    match number with
      | None -> "interp"
      | Some n -> "interp" ^ string_of_int n in
  Tptp_prob.model_to_tptp ~prob_name tp model
    (Tptp_ast.N_word (Tptp_ast.to_plain_word formula_name))
    (fun f ->
      Tptp.write b f;
      BatBuffer.print out b;
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

let sat_solve (module Inst : Sat_inst.Inst_sig) tp sorts cfg =
  let print_instantiating dsize =
    print_with_time cfg (Printf.sprintf "Instantiating %d" dsize) in

  (* Remove adequate domain sizes when searching for all models.
     Function [Model.all_of_ms_model] can't handle domains
     with different sizes.
  *)
  let sorts =
    if not cfg.all_models then
      sorts
    else
      let adeq_sizes = Earray.map (fun _ -> 0) sorts.Sorts.adeq_sizes in
      { sorts with Sorts.adeq_sizes } in

  let p = tp.Tptp_prob.prob in
  let inst = Inst.create p sorts in
  let model_cnt = ref 0 in

  for dsize = 1 to cfg.n_from - 1 do
    print_instantiating dsize;
    Inst.incr_max_size inst
  done;

  if cfg.all_models then begin
    let dsize = cfg.n_from in
    print_instantiating dsize;
    Inst.incr_max_size inst;
    if dsize < (Symb.distinct_consts p.Prob.symbols |> Symb.Set.cardinal) then
      let () = write_summary cfg S_gave_up in
      Printf.fprintf stderr "\n"
    else begin
      let tot_ms_model_cnt = ref 0 in
      let write_summary s =
        if !tot_ms_model_cnt = 0 then
          write_summary cfg s in
      let ms_models = ref (BatSet.PSet.create Ms_model.compare) in
      let rec loop () =
        if not (has_time cfg) then
          let () = write_summary S_timeout in
          print_with_time cfg "\nTime out"
        else begin
          match call_solver cfg inst Inst.solve Inst.solve_timed with
            | Sh.Ltrue ->
                write_summary S_satisfiable;
                incr tot_ms_model_cnt;
                let ms_model = Inst.construct_model inst in
                Inst.block_model inst ms_model;
                let cano_ms_model = Ms_model.canonize ms_model sorts in
                if not (BatSet.PSet.mem cano_ms_model !ms_models) then begin
                  ms_models := BatSet.PSet.add cano_ms_model !ms_models;
                  let models = Model.all_of_ms_model cano_ms_model sorts in
                  BatSet.iter
                    (fun model ->
                      with_output
                        ~append:true
                        cfg
                        (write_model cfg.in_file tp model (Some !model_cnt));
                      incr model_cnt)
                    models
                end;
                loop ()
            | Sh.Lfalse ->
                (** No other models for this domain size exist. *)
                write_summary S_gave_up;
                Printf.fprintf stderr "\n"
            | Sh.Lundef ->
                write_summary S_timeout;
                print_with_time cfg "\nTime out"
        end in
      print_with_time cfg "Solving";
      with_output cfg (fun _ -> ()); (* Truncate file. *)
      loop ();
      Printf.fprintf stderr "%d multi-sorted models found\n" !tot_ms_model_cnt
    end
  end else begin
    let rec loop dsize =
      if dsize > cfg.n_to then
        let () = write_summary cfg S_gave_up in
        Printf.fprintf stderr "\n"
      else if not (has_time cfg) then
        let () = write_summary cfg S_timeout in
        print_with_time cfg "\nTime out"
      else begin
        print_instantiating dsize;
        Inst.incr_max_size inst;
        let result =
          if
            dsize < (Symb.distinct_consts p.Prob.symbols |> Symb.Set.cardinal)
          then
            Sh.Lfalse
          else
            let _ = print_with_time cfg "Solving" in
            call_solver cfg inst Inst.solve Inst.solve_timed in
        match result with
          | Sh.Ltrue ->
              write_summary cfg S_satisfiable;
              let ms_model = Inst.construct_model inst in
              let model = Model.of_ms_model ms_model sorts in
              with_output
                ~append:true
                cfg
                (write_model cfg.in_file tp model None);
              incr model_cnt;
              Printf.fprintf stderr "\n"
          | Sh.Lfalse -> loop (dsize + 1)
          | Sh.Lundef ->
              write_summary cfg S_timeout;
              print_with_time cfg "\nTime out"
      end in
    loop cfg.n_from
  end;

  match !model_cnt with
    | 0 -> print_with_time cfg "No model found"
    | 1 -> print_with_time cfg "1 model found"
    | n ->
        print_with_time
          cfg
          (Printf.sprintf "%d non-isomorphic models found" n)

let csp_solve (module Inst : Csp_inst.Inst_sig) tp cfg =
  let print_instantiating dsize =
    print_with_time cfg (Printf.sprintf "Instantiating %d" dsize) in

  let p = tp.Tptp_prob.prob in
  let model_cnt = ref 0 in

  if cfg.all_models then begin
    let dsize = cfg.n_from in
    if dsize < (Symb.distinct_consts p.Prob.symbols |> Symb.Set.cardinal) then
      let () = write_summary cfg S_gave_up in
      Printf.fprintf stderr "\n"
    else begin
      print_instantiating dsize;
      let inst = Inst.create ~nthreads:cfg.nthreads p dsize in
      let tot_model_cnt = ref 0 in
      let write_summary s =
        if !tot_model_cnt = 0 then
          write_summary cfg s in
      let models = ref (BatSet.PSet.create Model.compare) in
      let rec loop () =
        if not (has_time cfg) then
          let () = write_summary S_timeout in
          print_with_time cfg "\nTime out"
        else begin
          match call_solver cfg inst Inst.solve Inst.solve_timed with
            | Sh.Ltrue ->
                write_summary S_satisfiable;
                incr tot_model_cnt;
                let model = Inst.construct_model inst in
                let cano_model = Model.canonize model in
                if not (BatSet.PSet.mem cano_model !models) then begin
                  models := BatSet.PSet.add cano_model !models;
                  with_output
                    ~append:true
                    cfg
                    (write_model cfg.in_file tp model (Some !model_cnt));
                  incr model_cnt;
                end;
                loop ()
            | Sh.Lfalse ->
                write_summary S_gave_up;
                Printf.fprintf stderr "\n"
            | Sh.Lundef ->
                write_summary S_timeout;
                print_with_time cfg "\nTime out"
        end in
      print_with_time cfg "Solving";
      with_output cfg (fun _ -> ()); (* Truncate file. *)
      loop ();
      Printf.fprintf stderr "%d models found\n" !tot_model_cnt
    end
  end else begin
    let rec loop dsize =
      if dsize > cfg.n_to then
        let () = write_summary cfg S_gave_up in
        Printf.fprintf stderr "\n"
      else if not (has_time cfg) then
        let () = write_summary cfg S_timeout in
        print_with_time cfg "\nTime out"
      else if
        dsize < (Symb.distinct_consts p.Prob.symbols |> Symb.Set.cardinal)
      then
        loop (dsize + 1)
      else begin
        print_instantiating dsize;
        let inst =
          Inst.create  ~nthreads:cfg.nthreads p dsize in
        print_with_time cfg "Solving";
        match call_solver cfg inst Inst.solve Inst.solve_timed with
          | Sh.Ltrue ->
              write_summary cfg S_satisfiable;
              let model = Inst.construct_model inst in
              with_output
                ~append:true
                cfg
                (write_model cfg.in_file tp model None);
              incr model_cnt;
              Printf.fprintf stderr "\n"
          | Sh.Lfalse ->
              Inst.destroy inst;
              loop (dsize + 1)
          | Sh.Lundef ->
              write_summary cfg S_timeout;
              print_with_time cfg "\nTime out"
      end in
    loop cfg.n_from
  end;

  match !model_cnt with
    | 0 -> print_with_time cfg "No model found"
    | 1 -> print_with_time cfg "1 model found"
    | n ->
        print_with_time
          cfg
          (Printf.sprintf "%d non-isomorphic models found" n)

let minisat_solver =
  let s_func tp sorts cfg =
    sat_solve (module Minisat_inst.Inst : Sat_inst.Inst_sig) tp sorts cfg in
  {
    s_func;
    s_only_flat_clauses = true;
    s_default_transforms = [
      T_detect_commutativity; T_rewrite_ground_terms; T_unflatten;
      T_define_ground_terms; T_flatten; T_paradox_mod_splitting;
    ];
  }

let cmsat_solver =
  let s_func tp sorts cfg =
    sat_solve (module Cmsat_inst.Inst : Sat_inst.Inst_sig) tp sorts cfg in
  {
    s_func;
    s_only_flat_clauses = true;
    s_default_transforms = [
      T_detect_commutativity; T_rewrite_ground_terms; T_unflatten;
      T_define_ground_terms; T_flatten; T_paradox_mod_splitting;
    ];
  }

let josat_solver =
  let s_func tp sorts cfg =
    sat_solve (module Josat_inst.Inst : Sat_inst.Inst_sig) tp sorts cfg in
  {
    s_func;
    s_only_flat_clauses = true;
    s_default_transforms = [
      T_detect_commutativity; T_rewrite_ground_terms; T_unflatten;
      T_define_ground_terms; T_flatten; T_paradox_mod_splitting;
    ];
  }

let gecode_solver =
  let s_func tp _ cfg =
    csp_solve (module Gecode_inst.Inst : Csp_inst.Inst_sig) tp cfg in
  {
    s_func;
    s_only_flat_clauses = false;
    s_default_transforms = [
      T_detect_commutativity;
      T_detect_hints_for_groups; T_detect_hints_for_quasigroups;
      T_detect_hints_for_involutive_funcs;
      T_rewrite_ground_terms; T_unflatten;
    ];
  }

let only_preproc_solver =
  let s_func tp _ cfg =
    let b = Buffer.create 1024 in
    with_output cfg
      (fun out ->
        Tptp_prob.prob_to_tptp tp Tptp_prob.Export
          (fun f ->
            Tptp.write b f;
            BatBuffer.print out b;
            Buffer.clear b)) in
  {
    s_func;
    s_only_flat_clauses = false;
    s_default_transforms = [];
  }

type solver_id =
  | Solv_minisat
  | Solv_cmsat
  | Solv_josat
  | Solv_gecode
  | Solv_only_preproc

let all_solvers =
  [
    Solv_minisat, minisat_solver;
    Solv_cmsat, cmsat_solver;
    Solv_josat, josat_solver;
    Solv_gecode, gecode_solver;
    Solv_only_preproc, only_preproc_solver;
  ]

(* ************************************************************************ *)
(* Main *)

type clausifier =
  | C_none
  | C_generic
  | C_e

type lemma_gen =
  | LG_none
  | LG_e

let find_model
    clausifier
    clausifier_exe
    clausifier_opts
    lemma_gen
    lemma_gen_exe
    lemma_gen_opts
    lemma_gen_max_secs
    max_vars
    max_symbs
    max_vars_when_flat
    max_lits_when_flat
    max_lemmas
    detect_commutativity_from_lemmas
    transforms
    solver
    n_from
    n_to
    all_models
    nthreads
    max_secs
    disable_sort_inference
    verbose
    output_file
    base_dir
    in_file =

  let start_ms = Timer.get_ms () in
  let n_to = BatOption.default max_int n_to in
  if n_from < 1 || n_to < 1 then
    failwith "Minimal domain size is 1.";
  if nthreads < 0 then
    failwith "Invalid number of threads.";
  if lemma_gen_max_secs < 1 then
    failwith "Minimal time for lemma generator is 1 second.";
  let clausify =
    if clausifier <> C_none then begin
      let clausifier_exe =
        match clausifier_exe with
          | None -> failwith "Executable for clausifier not specified."
          | Some exe -> exe in
      match clausifier with
        | C_none -> failwith "impossible"
        | C_generic -> Generic_prover.run clausifier_exe clausifier_opts
        | C_e -> Eprover.clausify clausifier_exe clausifier_opts
    end else
      fun _ -> failwith "No clausifier specified" in
  let tptp_prob = Tptp_prob.of_file clausify base_dir in_file in
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
  (* Lemma generation. *)
  begin match lemma_gen with
    | LG_none -> ()
    | LG_e ->
        let lemma_gen_exe =
          match lemma_gen_exe with
            | None -> failwith "Executable for lemma generator not specified."
            | Some exe -> exe in
        let lemmas =
          generate_lemmas_by_e
            tptp_prob
            lemma_gen_exe
            lemma_gen_max_secs
            lemma_gen_opts
            detect_commutativity_from_lemmas
            max_vars
            max_symbs
            max_vars_when_flat
            max_lits_when_flat
            max_lemmas in
        (* Symbol properties (eg. commutativity) discovered by lemma
           generation won't be printed.
        *)
        print_lemmas (verbose >= 1) tptp_prob lemmas;
        BatDynArray.append lemmas p.Prob.clauses
  end;
  (* Preprocessing. *)
  List.iter
    (fun tname ->
      let t = List.assoc tname all_transforms in
      t.t_func p)
    transforms;
  (* Normalize variables. *)
  transform_each_clause p (fun cl -> [Clause.normalize_vars cl |> fst]);
  (* Infer sorts. *)
  let sorts =
    let sorts = Sorts.of_problem p in
    if disable_sort_inference
    then Sorts.unify_all sorts
    else sorts in
  (* Print statistics before solving. *)
  print_symb_info (verbose >= 2) tptp_prob;
  print_sort_info (verbose >= 2) sorts;
  print_clause_info (verbose >= 3) tptp_prob;
  flush stderr;
  (* Run selected solver. *)
  let cfg = {
    nthreads;
    all_models;
    n_from;
    n_to;
    in_file;
    has_conjecture = tptp_prob.Tptp_prob.has_conjecture;
    output_file;
    start_ms;
    max_ms = BatOption.map (fun secs -> secs * 1000) max_secs;
  } in
  if contains_empty_clause p then
    let () = write_summary cfg S_unsatisfiable in
    print_with_time cfg "\nNo model found - empty clause"
  else
    solver.s_func tptp_prob sorts cfg

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

let clausifier =
  let doc = "Clausifier. One of: none, generic, e." in
  let clausifiers = [
    "none", C_none;
    "generic", C_generic;
    "e", C_e;
  ] in
  Arg.(value & opt (enum clausifiers) C_none &
         info ["clausifier"] ~doc ~docs:"CLAUSIFICATION")

let clausifier_exe =
  let doc = "Clausifier executable." in
  Arg.(value & opt (some file) None &
         info ["clausifier-exe"] ~docv:"FILE" ~doc ~docs:"CLAUSIFICATION")

let clausifier_opts =
  let doc = "Pass the given option to clausifier." in
  Arg.(value & opt_all string [] &
         info ["clausifier-opt"] ~docv:"OPTION" ~doc ~docs:"CLAUSIFICATION")

let lemma_gen =
  let doc = "Lemma generator. One of: none, e." in
  let lemma_gens = [
    "none", LG_none;
    "e", LG_e;
  ] in
  Arg.(value & opt (enum lemma_gens) LG_none &
         info ["lemma-gen"] ~doc ~docs:"LEMMA GENERATION")

let lemma_gen_exe =
  let doc = "Lemma generator executable." in
  Arg.(value & opt (some file) None &
         info ["lemma-gen-exe"] ~docv:"FILE" ~doc ~docs:"LEMMA GENERATION")

let lemma_gen_opts =
  let doc = "Pass the given option to lemma generator." in
  Arg.(value & opt_all string [] &
         info ["lemma-gen-opt"] ~docv:"OPTION" ~doc ~docs:"LEMMA GENERATION")

let lemma_gen_max_secs =
  let doc = "Run lemma generator for at most $(docv) seconds." in
  Arg.(value & opt int 5 &
         info ["lemma-gen-max-secs"] ~docv:"N" ~doc ~docs:"LEMMA GENERATION")

let max_vars =
  let doc = "Remove lemmas with more than $(docv) different variables." in
  Arg.(value & opt int 7 &
         info ["max-vars"] ~docv:"N" ~doc ~docs:"LEMMA GENERATION")

let max_symbs =
  let doc =
    "Remove lemmas with more than $(docv) different symbols " ^
    "(except equality)." in
  Arg.(value & opt int 5 &
         info ["max-symbs"] ~docv:"N" ~doc ~docs:"LEMMA GENERATION")

let max_vars_when_flat =
  let doc =
    "Remove lemmas with more than $(docv) different variables " ^
    "when converted to flat form." in
  Arg.(value & opt int 7 &
         info ["max-vars-when-flat"] ~docv:"N" ~doc ~docs:"LEMMA GENERATION")

let max_lits_when_flat =
  let doc =
    "Remove lemmas with more than $(docv) literals " ^
    "when converted to flat form." in
  Arg.(value & opt int 6 &
         info ["max-lits-when-flat"] ~docv:"N" ~doc ~docs:"LEMMA GENERATION")

let max_lemmas =
  let doc =
    "Keep at most $(docv) lemmas with the smallest weights." in
  Arg.(value & opt int 20 &
         info ["max-lemmas"] ~docv:"N" ~doc ~docs:"LEMMA GENERATION")

let detect_commutativity_from_lemmas =
  let doc = "Detect commutativity from all lemmas (i.e. before removal)." in
  Arg.(value & opt bool true &
         info ["detect-commutativity-from-lemmas"]
           ~docv:"BOOL" ~doc ~docs:"LEMMA GENERATION")

let verbose =
  let doc = "Verbosity level of the program. $(docv) can be: 0, 1, 2, 3." in
  Arg.(value & opt int 1 & info ["v"; "verbose"] ~docv:"N" ~doc)

let disable_sort_inference =
  let doc = "Sort inference is enabled by default." in
  Arg.(value & flag & info ["disable-sort-inference"] ~doc)

let max_secs =
  let doc = "Stop search after $(docv) seconds." in
  Arg.(value & opt (some int) None &
         info ["max-secs"] ~docv:"N" ~doc)

let nthreads =
  let doc =
    "Number of threads. Zero means as many threads as processing units." in
  Arg.(value & opt int 0 & info ["threads"] ~docv:"N" ~doc)

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
  let doc =
    "$(docv) can be: cryptominisat, minisat, josat, gecode, only-preproc." in
  let values = [
    "cryptominisat", Solv_cmsat;
    "minisat", Solv_minisat;
    "josat", Solv_josat;
    "gecode", Solv_gecode;
    "only-preproc", Solv_only_preproc;
  ] in
  Arg.(value & opt (enum values) Solv_cmsat &
         info ["solver"] ~docv:"SOLVER" ~doc)

let transforms =
  let flags = [
    T_detect_commutativity,
    Arg.info ["detect-commutativity"] ~docs:"TRANSFORMATIONS";
    T_detect_hints_for_groups,
    Arg.info ["detect-hints-for-groups"] ~docs:"TRANSFORMATIONS";
    T_detect_hints_for_quasigroups,
    Arg.info ["detect-hints-for-quasigroups"] ~docs:"TRANSFORMATIONS";
    T_detect_hints_for_involutive_funcs,
    Arg.info ["detect-hints-for-involutive-funcs"] ~docs:"TRANSFORMATIONS";
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
  Term.(pure find_model $
          clausifier $ clausifier_exe $ clausifier_opts $
          lemma_gen $ lemma_gen_exe $ lemma_gen_opts $ lemma_gen_max_secs $
          max_vars $ max_symbs $ max_vars_when_flat $ max_lits_when_flat $
          max_lemmas $ detect_commutativity_from_lemmas $
          transforms $ solver $
          n_from $ n_to $ all_models $
          nthreads $ max_secs $ disable_sort_inference $ verbose $
          output_file $ base_dir $ in_file)

let info =
  let doc = "finite model finder" in
  Term.info "crossbow" ~version:"0.1" ~doc

let () =
  match Term.eval (find_model_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
