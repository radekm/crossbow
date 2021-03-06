(* Copyright (c) 2013, 2015 Radek Micek *)

module R = Report.Result
module RS = Run_shared

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let (%>) = BatPervasives.(%>)

let main
    (* Required command-line arguments. *)
    report config_name problems out_dir
    (* Optional command-line arguments. *)
    exe tptp_to_ladr_exe base_dir opts max_time max_mem =

  let each_problem file =
    (* Convert from TPTP to LADR. *)
    let in_tptp = BatFile.with_temporary_out (fun _ name -> name) in
    Tptp.File.write in_tptp (Tptp.File.read ~base_dir file);
    let in_ladr = BatFile.with_temporary_out (fun _ name -> name) in
    BatPervasives.with_dispose
      ~dispose:close_in
      (fun inp ->
        BatPervasives.with_dispose
          ~dispose:close_out
          (fun out ->
            let pid =
              Unix.create_process
                tptp_to_ladr_exe [| tptp_to_ladr_exe |]
                (Unix.descr_of_in_channel inp)
                (Unix.descr_of_out_channel out)
                (Unix.descr_of_out_channel stderr) in
            ignore (Unix.waitpid [] pid))
          (open_out in_ladr))
      (open_in in_tptp);
    Sys.remove in_tptp;
    (* Run Mace4. *)
    let args =
      Array.concat
        [
          Array.of_list opts;
        ] in
    let output_file =
      Shared.file_in_dir out_dir (Shared.file_name file ^ ".out") in
    let time, mem_peak, exit_status =
      BatPervasives.with_dispose
        ~dispose:close_in
        (fun inp ->
          BatPervasives.with_dispose
            ~dispose:close_out
            (fun out ->
              RS.run_solver_ex
                max_time max_mem exe args
                (Unix.descr_of_in_channel inp)
                (Unix.descr_of_out_channel out)
                (Unix.descr_of_out_channel stderr))
            (open_out output_file))
        (open_in in_ladr) in
    Sys.remove in_ladr;
    let model_size =
      match exit_status with
        | R.Exit_code _ when Sys.file_exists output_file ->
            BatFile.with_file_in output_file
              (fun inp ->
                let model_start line =
                  BatString.starts_with line
                    "============================== MODEL " in
                let s = "interpretation( " in
                let lines =
                  inp
                  |> BatIO.lines_of
                  |> BatEnum.drop_while (model_start %> not)
                  |> BatEnum.filter (fun l -> BatString.starts_with l s) in
                match BatEnum.peek lines with
                  | None -> None
                  | Some line ->
                      BatString.split line s
                      |> snd
                      |> (fun l -> BatString.split l ",")
                      |> fst
                      |> BatString.trim
                      |> (fun size_str -> Some (int_of_string size_str)))
        | R.Out_of_time
        | R.Out_of_memory
        | R.Exit_code _ -> None in
    { R.problem = file; R.time; R.mem_peak; R.exit_status; R.model_size } in

  RS.shared_main report config_name "mace4"
    opts max_time max_mem
    problems each_problem

let exe =
  let doc = "Mace4 executable." in
  Arg.(value & opt string (Shared.file_in_program_dir "mace4") &
         info ["exe"] ~docv:"FILE" ~doc)

let tptp_to_ladr_exe =
  let doc = "tptp_to_ladr executable." in
  Arg.(value & opt string (Shared.file_in_program_dir "tptp_to_ladr") &
         info ["tptp-to-ladr-exe"] ~docv:"FILE" ~doc)

let base_dir =
  let doc = "Relative include paths are resolved against this directory." in
  Arg.(value & opt dir "." & info ["base-dir"] ~docv:"DIR" ~doc)

let main_t =
  Term.(pure main $ RS.report $ RS.config_name $ RS.problems $ RS.out_dir $
          exe $ tptp_to_ladr_exe $ base_dir $
          RS.opts $ RS.max_time $ RS.max_mem)

let info =
  Term.info "run_mace4" ~version:RS.version

let () =
  match Term.eval (main_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
