(* Copyright (c) 2013, 2015 Radek Micek *)

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

let file_name file = Path.of_string file |> Path.name

let file_in_dir dir file =
  let dir = Path.of_string dir in
  Path.append dir file
  |> Path.to_ustring

let file_in_program_dir file =
  Path.of_string Sys.executable_name
  |> Path.map_name (fun _ -> file)
  |> Path.to_ustring

module Cg = struct

  (* Resource controllers aka subsystems. *)
  module Ctrl = struct
    module Cpuacct = struct
      (* Id of the controller. *)
      let id = "cpuacct"
      let path = Printf.sprintf "%s.%s" id

      let usage = path "usage"
    end

    module Memory = struct
      (* Id of the controller. *)
      let id = "memory"
      let path = Printf.sprintf "%s.%s" id

      let swappiness = path "swappiness"
      let failcnt = path "failcnt"
      let limit_in_bytes = path "limit_in_bytes"
      let usage_in_bytes = path "usage_in_bytes"
      let max_usage_in_bytes = path "max_usage_in_bytes"

      module Memsw = struct
        let path = Printf.sprintf "%s.memsw.%s" id

        let failcnt = path "failcnt"
        let limit_in_bytes = path "limit_in_bytes"
        let usage_in_bytes = path "usage_in_bytes"
        let max_usage_in_bytes = path "max_usage_in_bytes"
      end
    end
  end

  (* Executables. *)
  module Exe = struct
    let create = "cgcreate"
    let delete = "cgdelete"
    let get = "cgget"
    let set = "cgset"
    let exec = "cgexec"
  end

  (* Executes [prog] with [args] and returns its output in a string.

     Note: [prog] is automatically prepended to [args] so [prog] will be
     the zeroth argument when executing [prog].
  *)
  let run_and_return_output prog args =
    let args = Array.append [| prog |] args in
    let out_file = Filename.temp_file "out" "" in
    BatPervasives.with_dispose
      ~dispose:close_out
      (fun out ->
        let pid =
          Unix.create_process
            prog args
            Unix.stdin
            (Unix.descr_of_out_channel out)
            Unix.stderr in
        let _, proc_status = Unix.waitpid [] pid in
        match proc_status with
          | Unix.WEXITED 0 -> ()
          | Unix.WSIGNALED _
          | Unix.WSTOPPED _
          | Unix.WEXITED _ ->
              Sys.remove out_file;
              let msg =
                args
                |> Array.to_list
                |> String.concat " "
                |> Printf.sprintf "run_and_return_output: %s" in
              failwith msg)
      (open_out out_file);
    let output =
      out_file
      |> BatFile.lines_of
      |> BatList.of_enum
      |> BatString.concat "\n" in
    Sys.remove out_file;
    output

  let create controllers cg =
    let controllers = String.concat "," controllers in
    run_and_return_output Exe.create
      [|
        "-g";
          Printf.sprintf "%s:%s" controllers cg;
      |]
    |> ignore

  let delete controllers cg =
    let controllers = String.concat "," controllers in
    run_and_return_output Exe.delete
      [|
        Printf.sprintf "%s:%s" controllers cg;
      |]
    |> ignore

  let get cg p =
    run_and_return_output Exe.get [| "-vn"; "-r"; p; cg |]
    |> int_of_string

  let set cg p v =
    run_and_return_output Exe.set
      [|
        "-r";
        Printf.sprintf "%s=%d" p v;
        cg;
      |]
    |> ignore

  let set_and_verify cg p v =
    set cg p v;
    let real_v = get cg p in
    if real_v <> v then
      failwith (Printf.sprintf "set_and_verify: %s=%d" p v)

  (** Returns pid of the created process. *)
  let exec controllers cg prog args new_stdin new_stdout new_stderr =
    let controllers = String.concat "," controllers in
    let args =
      Array.append
        [|
          Exe.exec;
          "-g";
          Printf.sprintf "%s:%s" controllers cg;
          prog;
        |]
        args in
    Unix.create_process
      Exe.exec args
      new_stdin new_stdout new_stderr

end

let div_ceil x y =
  if x mod y = 0
  then x / y
  else x / y + 1

let s_of_ns ns = div_ceil ns (1000 * 1000 * 1000)

let mib_to_b mib = mib * (1024 * 1024)
let mib_of_b b = div_ceil b (1024 * 1024)

module Cg_stats = struct

  module Cpu = Cg.Ctrl.Cpuacct
  module Mem = Cg.Ctrl.Memory

  let setup_cgroup cg ~max_mem =
    let check_zero x =
      if Cg.get cg x <> 0 then
        failwith (Printf.sprintf "setup_cgroup: nonzero %s" x) in

    Cg.set_and_verify cg Mem.swappiness 0;

    BatOption.may
      (fun i ->
        let limit = mib_to_b i in
        Cg.set_and_verify cg Mem.limit_in_bytes limit;
        Cg.set_and_verify cg Mem.Memsw.limit_in_bytes limit)
      max_mem;

    check_zero Cpu.usage;

    check_zero Mem.usage_in_bytes;
    check_zero Mem.Memsw.usage_in_bytes;

    check_zero Mem.failcnt;
    check_zero Mem.Memsw.failcnt;

    check_zero Mem.max_usage_in_bytes;
    check_zero Mem.Memsw.max_usage_in_bytes

  let read cg =
    let cpu_usage = Cg.get cg Cpu.usage in

    let mem_failcnt = Cg.get cg Mem.Memsw.failcnt in
    let mem_max_usage = Cg.get cg Mem.Memsw.max_usage_in_bytes in

    let time = cpu_usage |> s_of_ns in
    let mem_peak = mem_max_usage |> mib_of_b in
    let mem_failure = mem_failcnt > 0 in
    time, mem_peak, mem_failure

end

let run_with_limits
    cgroup max_time max_mem
    prog args
    new_stdin new_stdout new_stderr =

  let controllers = [Cg.Ctrl.Cpuacct.id; Cg.Ctrl.Memory.id] in
  let cgroup = cgroup ^ "/crossbow-run_with_limits" in

  (* Delete cgroups. Deleting nonexistent cgroup results in error
     so cgroups are created before deletition.
  *)
  Cg.create controllers cgroup;
  Cg.delete controllers cgroup;

  Cg.create controllers cgroup;
  Cg_stats.setup_cgroup cgroup ~max_mem;

  let max_time = BatOption.default max_int max_time in
  let max_mem = BatOption.default max_int max_mem in

  let pid =
    Cg.exec controllers cgroup
      prog args
      new_stdin new_stdout new_stderr in

  let rec wait () =
    Unix.sleep 1;
    let res, proc_status = Unix.waitpid [Unix.WNOHANG] pid in
    (* [Cg_stats.read] should be called after [Unix.waitpid]. *)
    let time, mem_peak, mem_failure = Cg_stats.read cgroup in

    if res = -1 then
      failwith "run_with_limits: error when waiting"

    (* The process hasn't terminated yet. *)
    else if res = 0 then begin
      (* Kill the process if the time limit is exceeded. *)
      if time > max_time then begin
        try
          Unix.kill pid Sys.sigkill
        (* The process with [pid] doesn't exist. *)
        with Unix.Unix_error (Unix.ESRCH, _, _) -> ()
      end;
      wait ()

    (* The process has terminated. *)
    end else
      let exit_status =
        if mem_peak > max_mem then
          failwith "run_with_limits: memory limit exceeded - impossible";
        match proc_status with
          | Unix.WSIGNALED i when i = Sys.sigkill ->
              if time > max_time then
                Report.Result.Out_of_time
              else if mem_failure then
                Report.Result.Out_of_memory
              (* The process was killed but it isn't known why. *)
              else
                failwith "run_with_limits: WSIGNALED Sys.sigkill"
          | Unix.WSIGNALED i ->
              failwith (Printf.sprintf "run_with_limits: WSIGNALED %d" i)
          | Unix.WSTOPPED i ->
              failwith (Printf.sprintf "run_with_limits: WSTOPPED %d" i)
          | Unix.WEXITED code ->
              (* The time limit has been exceeded but the process terminated
                 before [run_with_limits] managed to kill it.
              *)
              if time > max_time then
                Report.Result.Out_of_time
              else
                Report.Result.Exit_code code in
      time, mem_peak, exit_status in

  let res = wait () in
  Cg.delete controllers cgroup;
  res
