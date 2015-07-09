(* Copyright (c) 2014-15 Radek Micek *)

let run e_exe e_opts inputs =
  (* Write input formulas to file. *)
  let in_file = Filename.temp_file "eprover" "in" in
  Tptp.File.write in_file inputs;

  (* Execute E prover. *)
  let out_file = Filename.temp_file "eprover" "out" in
  let args =
    BatArray.concat
      [
        [| e_exe |];
        BatArray.of_list e_opts;
        [| "--tstp-format"; "--silent" |];
        [| "--output-file=" ^ out_file |];
        [| in_file |];
      ] in
  let pid =
    Unix.create_process
      e_exe args
      Unix.stdin
      Unix.stdout
      Unix.stderr in
  begin match Unix.waitpid [] pid with
    | _, Unix.WEXITED 0
    (* E prover 1.8 may exit with code 8
       when used with parameter [soft-cpu-limit].
    *)
    | _, Unix.WEXITED 8
    (* E prover 1.8 may exit with code 1
       when the input is satisfiable.
    *)
    | _, Unix.WEXITED 1 -> ()
    | _, Unix.WEXITED i ->
        failwith (Printf.sprintf "E exited with code %d" i)
    | _ -> failwith "E failed"
  end;

  (* Remove # comments from E prover output. *)
  let out_file2 = Filename.temp_file "eprover" "out2" in
  let remove_comment line =
    try
      let before, _ = BatString.split line "#" in
      before
    with
      | Not_found -> line in
  out_file
  |> BatFile.lines_of
  |> BatEnum.map remove_comment
  |> BatFile.write_lines out_file2;

  let inputs = Tptp.File.read out_file2 in

  Sys.remove in_file;
  Sys.remove out_file;
  Sys.remove out_file2;

  inputs

let clausify e_exe e_opts inputs =
  let e_opts = e_opts @ [
    "--cnf";
  ] in
  run e_exe e_opts inputs

let generate_lemmas e_exe e_opts e_max_secs inputs =
  let e_opts = e_opts @ [
    "--print-saturated=ei";
    "--soft-cpu-limit=" ^ string_of_int e_max_secs;
  ] in
  run e_exe e_opts inputs
