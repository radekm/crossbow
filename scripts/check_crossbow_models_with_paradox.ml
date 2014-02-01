(* Copyright (c) 2014 Radek Micek *)

module Ast = Tptp_ast

let with_tptp_in (file : string) (f : Tptp.input -> 'a) : 'a =
  BatFile.with_file_in file
    (fun inp ->
      let lexbuf = BatLexing.from_input inp in
      BatPervasives.with_dispose
        ~dispose:Tptp.close_in
        f
        (Tptp.create_in lexbuf))

let iter_tptp_input
    (tptp_file : string)
    (f : Ast.tptp_input -> unit)
    : unit =
  let rec iter inp =
    match Tptp.read inp with
      | None -> ()
      | Some tptp_input ->
          f tptp_input;
          iter inp in

  with_tptp_in tptp_file iter

let iter_models
    (file_with_models : string)
    (f : int -> Ast.tptp_input list -> unit)
    : unit =
  let rec iter dsize model_formulas inp =
    match Tptp.read inp with
      | None ->
          if model_formulas <> [] then
            f dsize (List.rev model_formulas)
      (* Comment with domain size - each model starts with it. *)
      | Some (Ast.Comment cstr) ->
          if model_formulas <> [] then
            f dsize (List.rev model_formulas);
          let dsize =
            BatString.replace
              ~str:(cstr :> string)
              ~sub:" domain size: "
              ~by:""
            |> snd
            |> int_of_string in
          iter dsize [] inp
      | Some (Ast.Cnf_anno _)
      | Some (Ast.Include _) ->
          failwith "iter_models: unexpected tptp_input"
      | Some (Ast.Fof_anno af as tptp_input) ->
          iter dsize (tptp_input :: model_formulas) inp in

  with_tptp_in file_with_models (iter 0 [])

let check_model_with_paradox
    problem_file
    base_dir
    paradox_exe
    dsize
    model_formulas =
  let paradox_in = BatFile.with_temporary_out
    (fun out file ->
      let b = Buffer.create 1000 in

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
          Tptp.write b af;
          BatIO.nwrite out (Buffer.contents b);
          Buffer.clear b
        done
      done;

      (* Write model formulas. *)
      BatList.iter
        (function
          | Ast.Fof_anno af ->
              Tptp.write b
                (Ast.Fof_anno { af with Ast.af_role = Ast.R_axiom });
              BatIO.nwrite out (Buffer.contents b);
              Buffer.clear b
          | Ast.Comment _
          | Ast.Cnf_anno _
          | Ast.Include _ ->
              failwith "check_model_with_paradox: unexpected tptp_input")
        model_formulas;

      (* Write problem formulas. *)
      iter_tptp_input problem_file
        (fun tptp_input ->
          Tptp.write b tptp_input;
          BatIO.nwrite out (Buffer.contents b);
          Buffer.clear b);

      file) in

  (* Execute Paradox. *)
  let paradox_out = BatFile.with_temporary_out (fun _ file -> file) in
  BatPervasives.with_dispose
    ~dispose:close_out
    (fun out ->
      let pid = Unix.create_process
        paradox_exe
        [| paradox_exe; paradox_in; "--root"; base_dir |]
        Unix.stdin
        (Unix.descr_of_out_channel out)
        Unix.stderr in
      ignore (Unix.waitpid [] pid))
    (open_out paradox_out);

  (* Check that Paradox found model. *)
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

let main exe base_dir models problem =
  iter_models models (check_model_with_paradox problem base_dir exe)

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
  Term.(pure main $ exe $ base_dir $ models $ problem)

let info =
  Term.info "check_crossbow_models_with_paradox"

let () =
  match Term.eval (main_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
