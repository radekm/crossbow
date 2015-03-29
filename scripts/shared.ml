(* Copyright (c) 2013 Radek Micek *)

module Path = BatPathGen.OfString

let (%>) = BatPervasives.(%>)

let read_problems_of_file file =
  let problems =
    BatFile.lines_of file
    |> BatEnum.map BatString.trim
    |> BatEnum.filter (fun file -> file <> "")
    |> BatList.of_enum in

  let cnt_unique_names =
    problems
    |> BatList.map (Path.of_string %> Path.name)
    |> BatList.sort_unique compare
    |> List.length in

  if List.length problems > cnt_unique_names then
    failwith "two files with problems have the same name";

  let _ =
    try
      let file = List.find (Sys.file_exists %> not) problems in
      failwith ("problem doesn't exist: " ^ file)
    with
      | Not_found -> () in

  let _ =
    try
      let file = List.find Sys.is_directory problems in
      failwith ("problem isn't file: " ^ file)
    with
      | Not_found -> () in

  problems

let get_ms () =
  let ns = Int64.to_int (Oclock.gettime Oclock.monotonic_raw) in
  ns / 1000000

let file_name file = Path.of_string file |> Path.name

let file_in_dir dir file =
  let dir = Path.of_string dir in
  Path.append dir file
  |> Path.to_ustring

let file_in_program_dir file =
  Path.of_string Sys.executable_name
  |> Path.map_name (fun _ -> file)
  |> Path.to_ustring

let div_ceil x y =
  if x mod y = 0
  then x / y
  else x / y + 1

let s_of_ms ms = div_ceil ms 1000

let mib_to_kib mib = mib * 1024
let mib_of_kib kib = div_ceil kib 1024

(* Note: The timeout script measures the time which the process has spent
   on the CPU not the wall clock time. But the function [run_with_limits]
   returns the wall clock time.

   This means that a process running on [n] cores simultaneously
   will be interrupted after [max_time / n] seconds of a wall clock time
   which is [max_time] of a CPU time.
*)
let run_with_limits
    timeout_exe max_time max_mem
    prog args
    new_stdin new_stdout new_stderr =

  let stats_file = BatFile.with_temporary_out (fun _ name -> name) in
  let args =
    Array.concat
      [
        [| timeout_exe |];
        [| "-c"; "-o"; stats_file |];
        BatOption.map_default
          (fun max_time -> [| "-t"; string_of_int max_time |])
          [| |]
          max_time;
        (* The script accepts a memory limit in kibibytes. *)
        BatOption.map_default
          (fun max_mem -> [| "-m"; string_of_int (mib_to_kib max_mem) |])
          [| |]
          max_mem;
        [| "--"; prog |];
        args;
      ] in
  let ms_before = get_ms () in
  let pid =
    Unix.create_process
      timeout_exe args
      new_stdin new_stdout new_stderr in
  let _, proc_status = Unix.waitpid [] pid in
  let ms = get_ms () - ms_before in
  match proc_status with
    | Unix.WSIGNALED _
    | Unix.WSTOPPED _ ->
        Sys.remove stats_file;
        failwith "run_with_limits: process status"
    | Unix.WEXITED code ->
        let stats =
          BatFile.lines_of stats_file
          |> BatEnum.map BatString.trim
          |> BatEnum.filter (fun line -> line <> "")
          |> BatList.of_enum in
        Sys.remove stats_file;
        match stats with
          | [reason; _; _; mem_peak] ->
              let s = s_of_ms ms in
              let mem_peak =
                BatString.lchop ~n:7 mem_peak
                |> int_of_string
                |> mib_of_kib in
              if reason = "REASON:FINISHED" then
                s, mem_peak, Report.Result.Exit_code code
              else if reason = "REASON:TIMEOUT" then
                s, mem_peak, Report.Result.Out_of_time
              else if reason = "REASON:MEM" then
                s, mem_peak, Report.Result.Out_of_memory
              else
                failwith "run_with_limits: reason"
          | _ -> failwith "run_with_limits: statistics"
