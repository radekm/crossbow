(* Copyright (c) 2013 Radek Micek *)

open Report_reader

module Path = BatPathGen.OfString

let (|>) = BatPervasives.(|>)
let (%>) = BatPervasives.(%>)

(* Generates a table like this:

           | Config    Config    Config
           | name      name      name
-----------------------------------------
 Problem - | time    | time    | time
 without   | in s    | in s    | in s
 extension |         |         |
-----------------------------------------
 Problem - | time    | time    | time
 without   | in s    | in s    | in s
 extension |         |         |
-----------------------------------------

*)
let results_to_latex output_file reports =
  (* Read reports given at command line. *)
  let rec read_reports acc = function
    | [] -> List.rev acc;
    | config_name :: report_dir :: results ->
        let report_file = Shared.file_in_dir report_dir report_file_name in
        if not (Sys.file_exists report_file) then
          failwith "file with report not found";
        read_reports (report_from_file config_name report_file :: acc) results
    | _ -> failwith "missing directory with report" in

  (* Check that reports were created with same settings
     and contain same problems.
  *)
  let check_reports = function
    | [] -> failwith "no reports given"
    | rep :: reports ->
        if
          List.exists (fun r -> r.max_time <> rep.max_time) reports ||
          List.exists (fun r -> r.max_mem <> rep.max_mem) reports
        then
          failwith "different settings";
        let get_problems rep = BatList.map (fun r -> r.problem) rep.results in
        let problems = get_problems rep in
        if List.exists (fun r -> get_problems r <> problems) reports then
          failwith "different problems"  in

  if Sys.file_exists output_file then
    failwith "output file already exists";

  let reports = read_reports [] reports in
  check_reports reports;

  (* Row contains all results for single problem. *)
  let results_in_rows =
    let rec to_rows acc_rows = function
      (* Rows are reversed. *)
      | [] -> BatList.map List.rev acc_rows
      | rep :: reports ->
          (* Add each result to corresponding row. *)
          let new_acc = BatList.map2 BatList.cons rep.results acc_rows in
          to_rows new_acc reports in
    match reports with
      | [] -> failwith "impossible"
      | r :: _ -> to_rows (BatList.make (List.length r.results) []) reports in

  (* Formatting. *)
  let unimp = Printf.sprintf "{\\footnotesize %s}" in
  let imp = Printf.sprintf "\\textbf{%s}" in
  let unimp_int = string_of_int %> unimp in
  let imp_int = string_of_int %> imp in

  let write_header o =
    BatIO.nwrite o "\\begin{longtable}{l";
    List.iter (fun _ -> BatIO.nwrite o "|c") reports;
    BatIO.nwrite o "}\n";
    List.iter
      (fun rep ->
        BatIO.nwrite o " & ";
        BatIO.nwrite o "\\multicolumn{1}{c}{\\adjustbox{angle=90}{";
        BatIO.nwrite o (unimp rep.config_name);
        BatIO.nwrite o "}}")
      reports;
    BatIO.nwrite o "\\\\\n";
    BatIO.nwrite o "\\hline\n";
    BatIO.nwrite o "\\endhead\n" in

  let write_results o =
    List.iter
      (fun row ->
        match row with
          | [] -> failwith "impossible"
          | result :: _ ->
              let model_sizes =
                row
                |> BatList.filter model_found
                |> BatList.filter_map (fun res -> res.model_size)
                |> BatList.filter (fun s -> s > 0) in
              (* Check that all models have equal size
                 (ignore non-positive sizes).
              *)
              begin match model_sizes with
                | [] -> ()
                | size :: rest ->
                    if List.exists (fun s -> s <> size) rest then
                      failwith ("different model size, problem " ^
                                 result.problem)
              end;
              let min_time =
                row
                |> BatList.filter model_found
                |> BatList.map (fun res -> res.time)
                |> List.fold_left min max_int
                |> milisecs_to_secs in
              let name = Path.of_string result.problem |> Path.name_core in
              (* Write row. *)
              BatIO.nwrite o (unimp name);
              List.iter
                (fun res ->
                  BatIO.nwrite o " & ";
                  BatIO.nwrite
                    o
                    (match res.exit_status with
                      | Out_of_time -> unimp "t"
                      | Out_of_memory -> unimp "m"
                      | Exit_code 0 when res.model_size <> None ->
                          let time = milisecs_to_secs res.time in
                          if time = min_time then
                            imp_int time
                          else
                            unimp_int time
                      | Exit_code _ -> unimp "x"))
                row;
              BatIO.nwrite o "\\\\\n";
              BatIO.nwrite o "\\hline\n")
      results_in_rows in

  let write_footer o =
    BatIO.nwrite o "\\end{longtable}\n" in

  BatFile.with_file_out
    output_file
    (fun o ->
      BatIO.nwrite o "\\documentclass[a4paper]{article}\n";
      BatIO.nwrite o "\\usepackage[utf8]{inputenc}\n";
      BatIO.nwrite o "\\usepackage{longtable}\n";
      BatIO.nwrite o "\\usepackage{adjustbox}\n";
      BatIO.nwrite o "\\begin{document}\n";
      write_header o;
      write_results o;
      write_footer o;
      BatIO.nwrite o "\\end{document}\n";
    )

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let output_file =
  let doc = "Write results to this file." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTPUT" ~doc)

let reports =
  let doc = "Pairs: configuration name and directory with report." in
  Arg.(value & pos_right 0 string [] & info [] ~docv:"REPORTS" ~doc)

let results_to_latex_t =
  Term.(pure results_to_latex $ output_file $ reports)

let info =
  Term.info "results_to_latex"

let () =
  match Term.eval (results_to_latex_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
