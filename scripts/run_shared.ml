(* Copyright (c) 2013, 2015 Radek Micek *)

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

module Cfg = Report.Config
module Res = Report.Result

let shared_main
    report config_name solver
    opts max_time max_mem
    problems f =
  BatOption.may
    (fun max_time ->
      if max_time < 1 then
        failwith "invalid time limit")
    max_time;

  BatOption.may
    (fun max_mem ->
      if max_mem < 1 then
        failwith "invalid memory limit")
    max_mem;

  let problems = Shared.read_problems_of_file problems in

  Cfg.ensure report {
    Cfg.name = config_name;
    Cfg.solver;
    Cfg.opts;
    Cfg.max_time;
    Cfg.max_mem;
  };

  List.iter
    (fun file ->
      Printf.fprintf stderr "\n%s\n" (BatString.repeat " -" 39);
      Printf.fprintf stderr "%s\n" (BatString.repeat "' " 39);
      if Res.get report config_name file = None then begin
        Printf.fprintf stderr "Solving (%s): %s\n" config_name file;
        flush stderr;
        let res = f file in
        Res.insert report config_name res;
        Printf.fprintf stderr
          "\nTIME (ms): %d  MEM PEAK (kB): %d  MODEL SIZE: %s\n"
          res.Res.time
          res.Res.mem_peak
          (BatOption.map_default string_of_int "no model" res.Res.model_size);
        Printf.fprintf stderr
          "EXIT: %s\n"
          (match res.Res.exit_status with
            | Res.Out_of_time -> "out of time"
            | Res.Out_of_memory -> "out of memory"
            | Res.Exit_code i -> string_of_int i);
        flush stderr
      end else begin
        Printf.fprintf stderr "Skipping (%s): %s\n" config_name file;
        flush stderr
      end)
    problems

let run_solver_ex
    max_time max_mem solver args
    new_stdin new_stdout new_stderr =

  let timeout_exe = Shared.file_in_program_dir "timeout" in
  Printf.fprintf stderr "\n+ %s %s\n\n"
    solver
    (args |> Array.to_list |> String.concat " ");
  flush stderr;
  Shared.run_with_limits
    timeout_exe max_time max_mem
    solver args
    new_stdin new_stdout new_stderr

let run_solver max_time max_mem solver args =
  run_solver_ex
    max_time max_mem solver args
    (Unix.descr_of_in_channel stdin)
    (Unix.descr_of_out_channel stdout)
    (Unix.descr_of_out_channel stderr)

let opts =
  let doc = "Pass the given option to the solver." in
  Arg.(value & opt_all string [] & info ["opt"] ~docv:"OPTION" ~doc)

let max_time =
  let doc = "Time limit for the solver (in seconds)." in
  Arg.(value & opt (some int) None &
         info ["max-time"] ~docv:"MAX-TIME" ~doc)

let max_mem =
  let doc = "Memory limit for the solver (in kilobytes)." in
  Arg.(value & opt (some int) None &
         info ["max-mem"] ~docv:"MAX-MEM" ~doc)

let report =
  let doc =
    "Report where the results will be stored. Report is a SQLite database." in
  Arg.(required & pos 0 (some string) None &
         info [] ~docv:"REPORT" ~doc)

let config_name =
  let doc = "Name of the current configuration." in
  Arg.(required & pos 1 (some string) None &
         info [] ~docv:"CONFIG-NAME" ~doc)

let problems =
  let doc = "File with a list of problems in TPTP CNF format." in
  Arg.(required & pos 2 (some non_dir_file) None &
         info [] ~docv:"PROBLEMS" ~doc)

let out_dir =
  let doc = "Directory for storing output of the solver." in
  Arg.(required & pos 3 (some dir) None & info [] ~docv:"OUTPUT-DIR" ~doc)

let version = "0.1"
