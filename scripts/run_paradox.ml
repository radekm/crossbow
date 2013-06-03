(* Copyright (c) 2013 Radek Micek *)

module RS = Run_shared

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let (|>) = BatPervasives.(|>)
let (|-) = BatPervasives.(|-)

let main exe opts max_time max_mem problems out_dir =
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
    let s_time, s_mem_peak, s_exit_status =
      BatPervasives.with_dispose
        ~dispose:close_out
        (fun out ->
          RS.run_solver_ex
            max_time max_mem exe args
            (Unix.descr_of_in_channel stdin)
            (Unix.descr_of_out_channel out)
            (Unix.descr_of_out_channel stderr))
        (open_out output_file) in
    let s_model_size =
      match s_exit_status with
        | Shared.ES_ok _ when Sys.file_exists output_file ->
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
        | Shared.ES_time
        | Shared.ES_memory
        | Shared.ES_ok _ -> None in
    { RS.s_time; RS.s_mem_peak; RS.s_exit_status; RS.s_model_size } in

  RS.shared_main "paradox" opts max_time max_mem problems out_dir each_problem

let exe =
  let doc = "Paradox executable." in
  Arg.(value & opt string (Shared.file_in_program_dir "paradox") &
         info ["exe"] ~docv:"FILE" ~doc)

let main_t =
  Term.(pure main $ exe $ RS.opts $ RS.max_time $ RS.max_mem $
          RS.problems $ RS.out_dir)

let info =
  Term.info "run_paradox" ~version:RS.version

let () =
  match Term.eval (main_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
