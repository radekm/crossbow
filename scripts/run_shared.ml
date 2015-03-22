(* Copyright (c) 2013, 2015 Radek Micek *)

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

module R = Report

let shared_main config_name solver opts max_time max_mem problems out_dir f =
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

  if Sys.readdir out_dir <> [| |] then
    failwith "output directory not empty";

  let problems = Shared.read_problems_of_file problems in

  let results = BatDynArray.create () in

  List.iter
    (fun file ->
      Printf.fprintf stderr "\n%s\n" (BatString.repeat " -" 39);
      Printf.fprintf stderr "%s\n" (BatString.repeat "' " 39);
      Printf.fprintf stderr "Solving: %s\n" file;
      flush stderr;
      let res = f file in
      BatDynArray.add results res;
      Printf.fprintf stderr
        "\nTIME (ms): %d  MEM PEAK (kB): %d  MODEL SIZE: %s\n"
        res.R.time
        res.R.mem_peak
        (BatOption.map_default string_of_int "no model" res.R.model_size);
      Printf.fprintf stderr
        "EXIT: %s\n"
        (match res.R.exit_status with
          | R.Out_of_time -> "out of time"
          | R.Out_of_memory -> "out of memory"
          | R.Exit_code i -> string_of_int i);
      flush stderr)
    problems;

  R.write
    (Shared.file_in_dir out_dir R.default_filename)
    {
      R.config_name;
      R.solver;
      R.opts;
      R.max_time;
      R.max_mem;
      R.results = BatDynArray.to_list results;
    }

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

let config_name =
  let doc = "Name of the current configuration." in
  Arg.(required & pos 0 (some string) None &
         info [] ~docv:"CONFIG-NAME" ~doc)

let problems =
  let doc = "File with a list of problems in TPTP CNF format." in
  Arg.(required & pos 1 (some non_dir_file) None &
         info [] ~docv:"PROBLEMS" ~doc)

let out_dir =
  let doc = "Empty directory for storing results." in
  Arg.(required & pos 2 (some dir) None & info [] ~docv:"OUTPUT-DIR" ~doc)

let version = "0.1"
