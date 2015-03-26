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
    let output_file =
      Shared.file_in_dir out_dir (Shared.file_name file ^ ".out") in
    let args =
      Array.concat
        [
          Array.of_list opts;
          [| "--tstp"; "--model" |];
          [| file |];
        ] in
    let time, mem_peak, exit_status =
      BatPervasives.with_dispose
        ~dispose:close_out
        (fun out ->
          RS.run_solver_ex
            max_time max_mem exe args
            (Unix.descr_of_in_channel stdin)
            (Unix.descr_of_out_channel out)
            (Unix.descr_of_out_channel stderr))
        (open_out output_file) in
    let model_size =
      match exit_status with
        | R.Exit_code _ when Sys.file_exists output_file ->
            BatFile.with_file_in output_file
              (fun inp ->
                let s = "% domain size is " in
                let lines =
                  inp
                  |> BatIO.lines_of
                  |> BatEnum.filter (fun l -> BatString.starts_with l s) in
                match BatEnum.peek lines with
                  | None -> None
                  | Some line ->
                      BatString.split line s
                      |> snd
                      |> BatString.trim
                      |> (fun size_str -> Some (int_of_string size_str)))
        | R.Out_of_time
        | R.Out_of_memory
        | R.Exit_code _ -> None in
    { R.problem = file; R.time; R.mem_peak; R.exit_status; R.model_size } in

  RS.shared_main report config_name "paradox"
    opts max_time max_mem
    problems each_problem

let exe =
  let doc = "Paradox executable." in
  Arg.(value & opt string (Shared.file_in_program_dir "paradox") &
         info ["exe"] ~docv:"FILE" ~doc)

let main_t =
  Term.(pure main $ RS.report $ RS.config_name $ RS.problems $ RS.out_dir $
          exe $ RS.opts $ RS.max_time $ RS.max_mem)

let info =
  Term.info "run_paradox" ~version:RS.version

let () =
  match Term.eval (main_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
