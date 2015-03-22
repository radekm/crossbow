(* Copyright (c) 2013 Radek Micek *)

module Path = BatPathGen.OfString

let (|>) = BatPervasives.(|>)
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
        BatOption.map_default
          (fun max_mem -> [| "-m"; string_of_int max_mem |])
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
              let mem_peak = int_of_string (BatString.lchop ~n:7 mem_peak) in
              if reason = "REASON:FINISHED" then
                ms, mem_peak, Report.Exit_code code
              else if reason = "REASON:TIMEOUT" then
                ms, mem_peak, Report.Out_of_time
              else if reason = "REASON:MEM" then
                ms, mem_peak, Report.Out_of_memory
              else
                failwith "run_with_limits: reason"
          | _ -> failwith "run_with_limits: statistics"

let combine_paths (a : string) (b : string) : string =
  let a = Path.of_string a in
  let b = Path.of_string b in
  let res =
    if Path.is_absolute b
    then b
    else (Path.concat a b) in
  Path.to_ustring
    (Path.normalize_in_tree res)

module Ast = Tptp_ast

let tptp_iter f base_dir file =

  let rec iter_commands proc_include proc_other i =
    match Tptp.read i with
      | None -> ()
      | Some (Ast.Include (file_name, sel)) ->
          proc_include file_name sel;
          iter_commands proc_include proc_other i
      | Some cmd ->
          proc_other cmd;
          iter_commands proc_include proc_other i

  and tptp_iter file selected =
    BatFile.with_file_in file (fun i ->
      let lexbuf = BatLexing.from_input i in
      let proc_include (file : Ast.file_name) sel =
        let path = combine_paths base_dir (file :> string) in
        let selected' =
          if sel = [] then
            selected
          else
            (fun name ->
              selected name && List.exists (fun x -> x = name) sel) in
        tptp_iter path selected' in
      let proc_other cmd =
        match cmd with
          | Ast.Include _ -> failwith "tptp_iter: include"
          | Ast.Fof_anno af -> if selected af.Ast.af_name then f cmd
          | Ast.Cnf_anno af -> if selected af.Ast.af_name then f cmd
          | Ast.Comment _ -> f cmd in
      BatPervasives.with_dispose
        ~dispose:Tptp.close_in
        (iter_commands proc_include proc_other)
        (Tptp.create_in lexbuf)) in

  tptp_iter file (fun _ -> true)

let tptp_concat_map f base_dir in_files out_file =
  BatFile.with_file_out out_file
    (fun out ->
      let b = Buffer.create 1024 in
      let proc_command cmd =
        Tptp.write b (f cmd);
        BatBuffer.print out b;
        Buffer.clear b in
      List.iter
        (tptp_iter proc_command base_dir)
        in_files)
