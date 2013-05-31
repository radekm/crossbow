(* Copyright (c) 2013 Radek Micek *)

module RS = Run_shared

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let (|>) = BatPervasives.(|>)
let (|-) = BatPervasives.(|-)

let main exe opts max_mem problems out_dir =
  let each_problem file =
    let model_file =
      Shared.file_in_dir out_dir (Shared.file_name file ^ ".m.mod") in
    let args =
      Array.concat
        [
          Array.of_list opts;
          [| "--output-file"; model_file |];
          [| file |];
        ] in
    let s_time, s_mem_peak, s_exit_code = RS.run_solver max_mem exe args in
    let s_model = Sys.file_exists model_file in
    { RS.s_time; RS.s_mem_peak; RS.s_exit_code; RS.s_model } in

  RS.shared_main "crossbow" opts max_mem problems out_dir each_problem

let exe =
  let doc = "Crossbow executable." in
  Arg.(value & opt string (Shared.file_in_program_dir "crossbow") &
         info ["exe"] ~docv:"FILE" ~doc)

let main_t =
  Term.(pure main $ exe $ RS.opts $ RS.max_mem $ RS.problems $ RS.out_dir)

let info =
  Term.info "run_crossbow" ~version:RS.version

let () =
  match Term.eval (main_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
