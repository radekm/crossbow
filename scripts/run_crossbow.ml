(* Copyright (c) 2013, 2015 Radek Micek *)

module R = Report.Result
module RS = Run_shared

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let main
    (* Required command-line arguments. *)
    report config_name problems out_dir
    (* Optional command-line arguments. *)
    exe opts max_time max_mem =
  let each_problem file =
    let model_file =
      Shared.file_in_dir out_dir (Shared.file_name file ^ ".m.mod") in
    let args =
      Array.concat
        [
          Array.of_list opts;
          [| "--output-file"; model_file |];
          [| file |];
        ] in
    let time, mem_peak, exit_status =
      RS.run_solver max_time max_mem exe args in
    let model_size =
      match exit_status with
        | R.Exit_code _ when Sys.file_exists model_file ->
            BatFile.with_file_in model_file
              (fun inp ->
                let line = BatIO.read_line inp in
                let size_str =
                  BatString.split line ":" |> snd |> BatString.trim in
                Some (int_of_string size_str))
        | R.Out_of_time
        | R.Out_of_memory
        | R.Exit_code _ -> None in
    { R.problem = file; R.time; R.mem_peak; R.exit_status; R.model_size } in

  RS.shared_main report config_name "crossbow"
    opts max_time max_mem
    problems each_problem

let exe =
  let doc = "Crossbow executable." in
  Arg.(value & opt string (Shared.file_in_program_dir "crossbow") &
         info ["exe"] ~docv:"FILE" ~doc)

let main_t =
  Term.(pure main $ RS.report $ RS.config_name $ RS.problems $ RS.out_dir $
          exe $ RS.opts $ RS.max_time $ RS.max_mem)

let info =
  Term.info "run_crossbow" ~version:RS.version

let () =
  match Term.eval (main_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
