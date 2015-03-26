(* Copyright (c) 2014-2015 Radek Micek *)

module Res = Report.Result
module RS = Run_shared
module Ast = Tptp_ast

let iter_models
    (file_with_models : string)
    (f : int -> Ast.tptp_input list -> unit)
    : unit =
  let dsize = ref 0 in
  let model_formulas = ref [] in
  Tptp.File.iter
    (function
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

      (* Write model formulas. *)
      BatList.iter
        (function
          | Ast.Fof_anno af ->
              let tptp_input =
                Ast.Fof_anno { af with Ast.af_role = Ast.R_axiom } in
              BatIO.nwrite out (Tptp.to_string tptp_input)
          | Ast.Comment _
          | Ast.Cnf_anno _
          | Ast.Include _ ->
              failwith "check_model_with_paradox: unexpected tptp_input")
        model_formulas;

      (* Write problem formulas. *)
      Tptp.File.iter
        ~base_dir
        (fun tptp_input ->
          BatIO.nwrite out (Tptp.to_string tptp_input))
        problem_file;

      file) in

  (* Execute Paradox. *)
  let paradox_out = BatFile.with_temporary_out (fun _ file -> file) in
  let _, _, exit_status =
    let timeout_exe = Shared.file_in_program_dir "timeout" in
    BatPervasives.with_dispose
      ~dispose:close_out
      (fun out ->
        Shared.run_with_limits
          timeout_exe max_time max_mem
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
              BatString.starts_with line "+++ RESULT: Satisfiable" in
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
