(* Copyright (c) 2013 Radek Micek *)

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let (|>) = BatPervasives.(|>)
let (|-) = BatPervasives.(|-)

type stat = {
  s_time : int;
  s_mem_peak : int;
  s_exit_code : int option;
  s_model : bool;
}

let shared_main solver_name opts max_mem problems out_dir f =
  if max_mem < 1 then
    failwith "invalid memory limit";

  if Sys.readdir out_dir <> [| |] then
    failwith "output directory not empty";

  let problems = Shared.read_problems_of_file problems in

  let stats = BatDynArray.create () in

  List.iter
    (fun file ->
      Printf.fprintf stderr "\n%s\n" (BatString.repeat " -" 39);
      Printf.fprintf stderr "%s\n" (BatString.repeat "' " 39);
      Printf.fprintf stderr "Solving: %s\n" file;
      flush stderr;
      let s = f file in
      BatDynArray.add stats (file, s);
      Printf.fprintf stderr
        "\nTIME (ms): %d  MEM PEAK (kB): %d  MODEL: %s\n"
        s.s_time
        s.s_mem_peak
        (if s.s_model then "yes" else "no");
      Printf.fprintf stderr
        "EXIT: %s\n"
        (match s.s_exit_code with
          | None -> "out of memory"
          | Some i -> string_of_int i);
      flush stderr)
    problems;

  let to_json (file, s) =
    let exit_code =
      match s.s_exit_code with
        | None -> `Null
        | Some i -> `Int i in
    `Assoc
      [
        "problem", `String file;
        "time", `Int s.s_time;
        "mem_peak", `Int s.s_mem_peak;
        "exit_code", exit_code;
        "model", `Bool s.s_model;
      ] in

  (* Save statistics to JSON. *)
  let json =
    let results =
      `List (BatDynArray.to_list stats |> BatList.map to_json) in
    `Assoc
      [
        "solver", `String solver_name;
        "opts", `List (BatList.map (fun opt -> `String opt) opts);
        "max_mem", `Int max_mem;
        "results", results;
      ] in
  BatFile.with_file_out
    (Shared.file_in_dir out_dir "__REPORT__")
    (fun out -> BatIO.write_string out (Yojson.Safe.pretty_to_string json))

let run_solver max_mem solver args =
  let timeout_exe = Shared.file_in_program_dir "timeout" in
  Printf.fprintf stderr "\n+ %s %s\n\n"
    solver
    (args |> Array.to_list |> String.concat " ");
  flush stderr;
  Shared.run_with_lim_mem
    timeout_exe max_mem
    solver args
    (Unix.descr_of_in_channel stdin)
    (Unix.descr_of_out_channel stdout)
    (Unix.descr_of_out_channel stderr)

let opts =
  let doc = "Pass the given option to the solver." in
  Arg.(value & opt_all string [] & info ["opt"] ~docv:"OPTION" ~doc)

let max_mem =
  let doc = "Memory limit for the solver (in kilobytes)." in
  Arg.(required & pos 0 (some int) None &
         info [] ~docv:"MAX-MEM" ~doc)

let problems =
  let doc = "File with a list of problems in TPTP CNF format." in
  Arg.(required & pos 1 (some non_dir_file) None &
         info [] ~docv:"PROBLEMS" ~doc)

let out_dir =
  let doc = "Empty directory for storing results." in
  Arg.(required & pos 2 (some dir) None & info [] ~docv:"OUTPUT-DIR" ~doc)

let version = "0.1"
