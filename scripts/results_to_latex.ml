(* Copyright (c) 2013 Radek Micek *)

module Path = BatPathGen.OfString

let (|>) = BatPervasives.(|>)

type exit_status =
  | Out_of_time
  | Out_of_memory
  | Exit_code of int

type result = {
  problem : string;
  time : int;
  mem_peak : int;
  exit_status : exit_status;
  model_size : int option;
}

type report = {
  config_name : string;
  solver : string;
  opts : string list;
  max_time : int option;
  max_mem : int option;
  results : result list;
}

let report_from_file config_name file =
  let read_string = function
    | `String s -> s
    | _ -> failwith "read_string" in
  let read_opts = function
    | `List opts -> BatList.map read_string opts
    | _ -> failwith "read_opts" in
  let read_int_or_null = function
    | `Int i -> Some i
    | `Null -> None
    | _ -> failwith "read_int_or_null" in
  let read_int = function
    | `Int i -> i
    | _ -> failwith "read_int" in
  let read_exit_status = function
    | `String "memory" -> Out_of_memory
    | `String "time" -> Out_of_time
    | `Int code -> Exit_code code
    | _ -> failwith "read_exit_status" in
  let read_result = function
    | `Assoc fields ->
        {
          problem = read_string (List.assoc "problem" fields);
          time = read_int (List.assoc "time" fields);
          mem_peak = read_int (List.assoc "mem_peak" fields);
          exit_status = read_exit_status (List.assoc "exit_status" fields);
          model_size = read_int_or_null (List.assoc "model_size" fields);
        }
    | _ -> failwith "read_result" in
  let read_results = function
    | `List results -> List.map read_result results
    | _ -> failwith "read_results" in
  match Yojson.Safe.from_file file with
    | `Assoc fields ->
        {
          config_name;
          solver = read_string (List.assoc "solver" fields);
          opts = read_opts (List.assoc "opts" fields);
          max_time = read_int_or_null (List.assoc "max_time" fields);
          max_mem = read_int_or_null (List.assoc "max_mem" fields);
          results =
            List.sort
              compare
              (read_results (List.assoc "results" fields));
        }
    | _ -> failwith "report_from_file"

let report_file_name = "__REPORT__"

(* Generates a table like this:

           | Config    Config    Config
           | name      name      name
------------------------------------------
 Problem - | time    | time    | time    |
 without   | in s    | in s    | in s    |
 extension |         |         |         |
------------------------------------------
 Problem - | time    | time    | time    |
 without   | in s    | in s    | in s    |
 extension |         |         |         |
------------------------------------------

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

  let write_header o =
    BatIO.nwrite o "\\begin{longtable}{l";
    List.iter (fun _ -> BatIO.nwrite o "|c") reports;
    BatIO.nwrite o "}\n";
    List.iter
      (fun rep ->
        BatIO.nwrite o " & ";
        BatIO.nwrite o
          "\\multicolumn{1}{c}{\\adjustbox{angle=90}{{\\footnotesize ";
        BatIO.nwrite o rep.config_name;
        BatIO.nwrite o "}}}")
      reports;
    BatIO.nwrite o "\\\\\n";
    BatIO.nwrite o "\\hline\n";
    BatIO.nwrite o "\\endhead\n" in

  let write_results o =
    let time_to_secs time =
      if time mod 1000 = 0
      then time / 1000
      else time / 1000 + 1 in

    List.iter
      (fun row ->
        match row with
          | [] -> failwith "impossible"
          | result :: _ ->
              (* Assumes that model was not found when the exit code <> 0. *)
              let model_found res =
                res.exit_status = Exit_code 0 &&
                res.model_size <> None in
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
                |> time_to_secs in
              let name = Path.of_string result.problem |> Path.name_core in
              (* Write row. *)
              BatIO.nwrite o (Printf.sprintf "{\\footnotesize %s}" name);
              List.iter
                (fun res ->
                  BatIO.nwrite o " & ";
                  BatIO.nwrite
                    o
                    (match res.exit_status with
                      | Out_of_time -> "{\\footnotesize t}"
                      | Out_of_memory -> "{\\footnotesize m}"
                      | Exit_code 0 when res.model_size <> None ->
                          let time = time_to_secs res.time in
                          if time = min_time then
                            Printf.sprintf "\\textbf{%d}" time
                          else
                            Printf.sprintf "{\\footnotesize %d}" time
                      | Exit_code _ -> "{\\footnotesize x}"))
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
