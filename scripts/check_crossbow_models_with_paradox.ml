(* Copyright (c) 2014-2015 Radek Micek *)

module Res = Report.Result
module RS = Run_shared
module Ast = Tptp_ast

module Symbols = struct

  type arity = int

  type symb =
    | Atomic_word of Ast.atomic_word * arity
    | Number of Q.t
    | String of Ast.tptp_string

  (** Calls [pred] resp. [func] for each occurence of a predicate
     resp. a function symbol in the input formula.

     Predicates [$false/0], [$true/0] and [=/2] are ignored.
     Occurences of symbols in annotations are ignored.
  *)
  let iter_symbols pred func input =
    let rec proc_term = function
      | Ast.Var _ -> ()
      | Ast.Func (symb, args) ->
          func (Atomic_word (symb, List.length args));
          List.iter proc_term args
      | Ast.Number n -> func (Number n)
      | Ast.String s -> func (String s) in
    let proc_atom = function
      | Ast.Equals (a, b) ->
          proc_term a;
          proc_term b
      | Ast.Pred (Ast.Defined_word s, [])
        when List.mem (s :> string) ["$false"; "$true"] -> ()
      | Ast.Pred (symb, args) ->
          pred (Atomic_word (symb, List.length args));
          List.iter proc_term args in
    let rec proc_formula = function
      | Ast.Binop (_, a, b) ->
          proc_formula a;
          proc_formula b
      | Ast.Not a | Ast.Quant (_, _, a) -> proc_formula a
      | Ast.Atom a -> proc_atom a in
    let proc_lit (Ast.Lit (_, a)) = proc_atom a in

    match input with
      | Ast.Fof_anno { Ast.af_formula } ->
          begin match af_formula with
            | Ast.Sequent (xs, ys) ->
                List.iter proc_formula xs;
                List.iter proc_formula ys
            | Ast.Formula x -> proc_formula x
          end
      | Ast.Cnf_anno { Ast.af_formula = Ast.Clause lits } ->
          List.iter proc_lit lits
      | Ast.Include _ -> failwith "iter_symbols: unexpected include"
      | Ast.Comment _ -> ()

  type t = {
    mutable preds : symb BatSet.t;
    mutable funcs : symb BatSet.t;
  }

  let create () = {
    preds = BatSet.empty;
    funcs = BatSet.empty;
  }

  let add symbs input =
    let add_pred s = symbs.preds <- BatSet.add s symbs.preds in
    let add_func s = symbs.funcs <- BatSet.add s symbs.funcs in
    iter_symbols add_pred add_func input

  let print_set desc symbs =
    let show_symb s =
      match s with
        | Atomic_word (aw, arity) ->
            Printf.sprintf "%s/%d" (Tptp_printer.show_atomic_word aw) arity
        | Number n -> Q.to_string n
        | String s -> Tptp_printer.show_tptp_string s in
    let strs = BatSet.to_list symbs |> BatList.map show_symb in
    if strs <> [] then
      Printf.printf "%s: %s\n" desc (String.concat ", " strs)

  (** Returns true if the model contains all symbols from the problem.

     Prints warning if the model doesn't contain all symbols
     from the problem.
     Prints warning if the model contains symbols except integers
     which aren't in the problem (integers are used as domain elements).
  *)
  let check ~symbs_in_model:m ~symbs_in_problem:p =
    let is_integer s =
      match s with
        | Number n -> n = Q.zero || Q.den n = Z.one
        | Atomic_word _
        | String _ -> false in
    let missing_preds = BatSet.diff p.preds m.preds in
    let missing_funcs = BatSet.diff p.funcs m.funcs in
    let additional_preds = BatSet.diff m.preds p.preds in
    let additional_funcs =
      BatSet.diff m.funcs p.funcs
      |> BatSet.filter (fun s -> not (is_integer s)) in
    print_set "Missing preds" missing_preds;
    print_set "Missing funcs" missing_funcs;
    print_set "Additional preds" additional_preds;
    print_set "Additional funcs" additional_funcs;
    BatSet.is_empty missing_preds && BatSet.is_empty missing_funcs

end

let iter_models
    (file_with_models : string)
    (f : int -> Ast.tptp_input list -> unit)
    : unit =
  let dsize = ref 0 in
  let model_formulas = ref [] in
  Tptp.File.iter
    (function
      (* Skip SZS data. *)
      | Ast.Comment cstr
        when BatString.starts_with (cstr :> string) " SZS " -> ()
      (* Comment with domain size - each model starts with it. *)
      | Ast.Comment cstr ->
          if !model_formulas <> [] then
            f !dsize (List.rev !model_formulas);
          dsize :=
            BatString.replace
              ~str:(cstr :> string)
              ~sub:" domain size: "
              ~by:""
            |> snd
            |> int_of_string;
          model_formulas := []
      | Ast.Cnf_anno _
      | Ast.Include _ ->
          failwith "iter_models: unexpected tptp_input"
      | Ast.Fof_anno _ as tptp_input ->
          model_formulas := tptp_input :: !model_formulas)
    file_with_models;

  (* Last model. *)
  if !model_formulas <> [] then
    f !dsize (List.rev !model_formulas)

let check_model_with_paradox
    problem_file
    base_dir
    paradox_exe
    max_time
    max_mem
    dsize
    model_formulas =

  let symbs_in_model = Symbols.create () in
  let symbs_in_problem = Symbols.create () in
  let paradox_in = BatFile.with_temporary_out
    (fun out file ->
      (* Paradox doesn't treat integer constants as distinct,
         we have to add inequalities.
      *)
      for i = 0 to dsize - 1 do
        for j = i + 1 to dsize - 1 do
          let term_i = Ast.Number (Q.of_int i) in
          let term_j = Ast.Number (Q.of_int j) in
          let eq = Ast.Equals(term_i, term_j) in
          let af = Ast.Fof_anno
            {
              Ast.af_name = Ast.N_word (Ast.to_plain_word "ineq");
              Ast.af_role = Ast.R_axiom;
              Ast.af_formula = Ast.Formula (Ast.Not (Ast.Atom eq));
              Ast.af_annos = None;
            } in
          BatIO.nwrite out (Tptp.to_string af);
        done
      done;

      (* Write model formulas and record symbols in model. *)
      BatList.iter
        (function
          | Ast.Fof_anno af ->
              let tptp_input =
                Ast.Fof_anno { af with Ast.af_role = Ast.R_axiom } in
              Symbols.add symbs_in_model tptp_input;
              BatIO.nwrite out (Tptp.to_string tptp_input)
          | Ast.Comment _
          | Ast.Cnf_anno _
          | Ast.Include _ ->
              failwith "check_model_with_paradox: unexpected tptp_input")
        model_formulas;

      (* Write problem formulas and record symbols in problem. *)
      Tptp.File.iter
        ~base_dir
        (fun tptp_input ->
          Symbols.add symbs_in_problem tptp_input;
          BatIO.nwrite out (Tptp.to_string tptp_input))
        problem_file;

      file) in

  (* Ensure that the model contains all symbols from the problem. *)
  if not (Symbols.check ~symbs_in_model ~symbs_in_problem) then begin
    flush stdout;
    Sys.remove paradox_in;
    failwith "Invalid model";
  end;

  (* Execute Paradox. *)
  let paradox_out = BatFile.with_temporary_out (fun _ file -> file) in
  let _, _, exit_status =
    let cgroup = "/crossbow-prover" in
    BatPervasives.with_dispose
      ~dispose:close_out
      (fun out ->
        Shared.run_with_limits
          cgroup max_time max_mem
          paradox_exe
          [| paradox_in |]
          Unix.stdin
          (Unix.descr_of_out_channel out)
          Unix.stderr)
      (open_out paradox_out) in

  (* Check that Paradox found model. *)
  begin match exit_status with
    | Res.Exit_code _ ->
        BatFile.with_file_in paradox_out
          (fun inp ->
            let is_satisfiable line =
              List.exists
                (BatString.starts_with line)
                [
                  "+++ RESULT: Satisfiable";
                  "+++ RESULT: CounterSatisfiable";
                ] in
            try
              while BatIO.read_line inp |> is_satisfiable |> not do
                ()
              done
            with
              | BatIO.No_more_input ->
                  Printf.printf "Invalid model: %s\n" paradox_in;
                  flush stdout;
                  Sys.remove paradox_out;
                  failwith "Invalid model");
        Sys.remove paradox_in;
        Sys.remove paradox_out
    | Res.Out_of_time ->
        Printf.printf "Out of time: %s\n" paradox_in;
        flush stdout;
        Sys.remove paradox_out;
    | Res.Out_of_memory ->
        Printf.printf "Out of memory: %s\n" paradox_in;
        flush stdout;
        Sys.remove paradox_out
  end

let main exe base_dir max_time max_mem models problem =
  iter_models
    models
    (check_model_with_paradox problem base_dir exe max_time max_mem)

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let exe =
  let doc = "Paradox executable." in
  Arg.(value & opt string (Shared.file_in_program_dir "paradox") &
         info ["exe"] ~docv:"FILE" ~doc)

let base_dir =
  let doc = "Relative include paths are resolved against this directory." in
  Arg.(value & opt dir "." & info ["base-dir"] ~docv:"DIR" ~doc)

let models =
  let doc = "File with Crossbow models in TPTP format." in
  Arg.(required & pos 0 (some non_dir_file) None &
         info [] ~docv:"MODELS" ~doc)

let problem =
  let doc = "File with the problem in TPTP format." in
  Arg.(required & pos 1 (some non_dir_file) None &
         info [] ~docv:"PROBLEM" ~doc)

let main_t =
  Term.(pure main $ exe $ base_dir $ RS.max_time $ RS.max_mem $
          models $ problem)

let info =
  Term.info "check_crossbow_models_with_paradox"

let () =
  match Term.eval (main_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
