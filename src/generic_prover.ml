(* Copyright (c) 2015 Radek Micek *)

let run exe opts inputs =
  (* Write input formulas to file. *)
  let in_file = Filename.temp_file "prover" "in" in
  Tptp.File.write in_file inputs;

  (* Execute prover. *)
  let out_file = Filename.temp_file "prover" "out" in
  let args =
    BatArray.concat
      [
        [| exe |];
        BatArray.of_list opts;
        [| in_file |];
      ] in
  let pid =
    BatPervasives.with_dispose
      ~dispose:Unix.close
      (fun out ->
        Unix.create_process
          exe args
          Unix.stdin
          out
          Unix.stderr)
      (Unix.openfile out_file [Unix.O_WRONLY] 0) in
  begin match Unix.waitpid [] pid with
    | _, Unix.WEXITED 0 -> ()
    | _, Unix.WEXITED i ->
        failwith (Printf.sprintf "Generic prover exited with code %d" i)
    | _ -> failwith "Generic prover failed"
  end;

  let inputs = Tptp.File.read out_file in

  Sys.remove in_file;
  Sys.remove out_file;

  inputs
