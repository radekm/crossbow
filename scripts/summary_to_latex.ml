(* Copyright (c) 2013 Radek Micek *)

module R = Report

open Report_reader

let (%>) = BatPervasives.(%>)

(* Expects that the problems were same for all solvers.

Generates a table like this:

         | No. of      Config             Config
         | problems    name               name
-----------------------------------------------------------
 Problem | count     | no. of solved    | no. of solved
 group   |           | problems         | problems
-----------------------------------------------------------
 Problem | count     | no. of solved    | no. of solved
 group   |           | problems         | problems
-----------------------------------------------------------
-----------------------------------------------------------
 Total   | total     | total no. of     | total no. of
         | count     | solved problems  | solved problems
-----------------------------------------------------------

*)
let count_solved_problems report time_limit =
  let max_time = BatOption.default max_int report.R.max_time in
  let max_mem = BatOption.default max_int report.R.max_mem in

  let rec loop cnt = function
    | [] -> cnt
    | res :: results ->
        let time = milisecs_to_secs res.R.time in
        if
          model_found res &&
          time <= time_limit &&
          time <= max_time &&
          res.R.mem_peak <= max_mem
        then
          loop (cnt+1) results
        else
          loop cnt results in
  loop 0 report.R.results

let summary_to_latex
    output_file ngroups items
    max_time
    label_num_problems label_total
    label_x label_y label_all_groups =

  if Sys.file_exists output_file then
    failwith "output file already exists";
  if ngroups <= 0 then
    failwith "number of problem groups is not positive";
  if items = [] then
    failwith "no items";
  if max_time < 1 then
    failwith "max time is not positive";

  let group_names, items2 = BatList.split_at ngroups items in
  let group_names = Array.of_list group_names in

  let rec read_reports acc = function
    | [] -> acc |> List.rev
    | rest ->
        let rep_dirs, rest = BatList.split_at ngroups rest in
        let reports =
          Array.init
            ngroups
            (fun i ->
              let dir = List.nth rep_dirs i in
              let report_file = Shared.file_in_dir dir R.default_filename in
              if not (Sys.file_exists report_file) then
                failwith "file with report not found";
              R.read report_file) in
        read_reports (reports :: acc) rest in

  (* Reports grouped by prover. *)
  let reports = read_reports [] items2 in

  (* Formatting. *)
  let unimp = Printf.sprintf "{\\footnotesize %s}" in
  let imp = Printf.sprintf "\\textbf{%s}" in
  let unimp_int = string_of_int %> unimp in
  let imp_int = string_of_int %> imp in

  let write_summary_header o =
    BatIO.nwrite o "\\begin{longtable}{l|c";
    List.iter (fun _ -> BatIO.nwrite o "|c") reports;
    BatIO.nwrite o "}\n";
    BatIO.nwrite o
      "& \\multicolumn{1}{c}{\\adjustbox{angle=90}{";
    BatIO.nwrite o (unimp label_num_problems);
    BatIO.nwrite o "}}";
    List.iter
      (fun reps ->
        BatIO.nwrite o " & ";
        BatIO.nwrite o "\\multicolumn{1}{c}{\\adjustbox{angle=90}{";
        BatIO.nwrite o (unimp reps.(0).R.config_name);
        BatIO.nwrite o "}}")
      reports;
    BatIO.nwrite o "\\\\\n";
    BatIO.nwrite o "\\hline\n";
    BatIO.nwrite o "\\endhead\n" in

  let write_summary_body o =
    for gr = 0 to ngroups - 1 do
      (* Group name. *)
      BatIO.nwrite o (unimp group_names.(gr));
      (* Number of problems. *)
      BatIO.nwrite o " & ";
      BatIO.nwrite o
        ((List.hd reports).(gr).R.results |> List.length |> unimp_int);
      let max_solved =
        List.fold_left
          (fun m reps -> max m (count_solved_problems reps.(gr) max_int))
          0
          reports in
      (* For each configuration: number of solved problems. *)
      List.iter
        (fun reps ->
          BatIO.nwrite o " & ";
          BatIO.nwrite
            o
            (let nsolved = count_solved_problems reps.(gr) max_int in
             if nsolved = max_solved then
               imp_int nsolved
             else
               unimp_int nsolved))
        reports;
      (* Finish row. *)
      BatIO.nwrite o "\\\\\n";
      BatIO.nwrite o "\\hline\n"
    done;
    let total_problems =
      Array.fold_left
        (fun sum rep -> sum + List.length rep.R.results)
        0
        (List.hd reports) in
    (* Number of total solved problems for each solver. *)
    let total_solved : int list =
      let count_total_solved reps =
        Array.fold_left
          (fun sum rep -> sum + count_solved_problems rep max_int)
          0
          reps in
      BatList.map count_total_solved reports in
    let max_solved = List.fold_left max 0 total_solved in
    BatIO.nwrite o "\\hline\n";
    BatIO.nwrite o (unimp label_total);
    BatIO.nwrite o " & ";
    BatIO.nwrite o (unimp_int total_problems);
    List.iter
      (fun n ->
        BatIO.nwrite o " & ";
        BatIO.nwrite
          o
          (if n = max_solved then
              imp_int n
           else
              unimp_int n))
      total_solved;
    BatIO.nwrite o "\\\\\n";
    BatIO.nwrite o "\\hline\n" in

  let write_summary_footer o =
    BatIO.nwrite o "\\end{longtable}\n" in

  let graph_step = 30 in

  let write_graph o caption reports =
    (* Header. *)
    BatIO.nwrite o "\n\n\n\n";
    BatIO.nwrite o "\\begin{figure}\n";
    BatIO.nwrite o "\\begin{tikzpicture}\n";
    BatIO.nwrite o "\\begin{axis}[";
    BatIO.nwrite o
      (String.concat ","
         [
           "width=12cm";
           "height=9cm";
           "legend style={at={(0.5,1.03)},anchor=south}";
           "legend columns=2";
           "xlabel=" ^ label_x;
           "ylabel=" ^ label_y;
         ]);
    BatIO.nwrite o "]\n";
    (* Plot for each solver. *)
    List.iter
      (fun reps ->
        BatIO.nwrite o "\\addplot coordinates {\n";
        for t = 1 to max_time do
          if t = 1 || t = max_time || t mod graph_step = 0 then begin
            let nsolved =
              Array.fold_left
                (fun sum rep -> sum + count_solved_problems rep t)
                0
                reps in
            BatIO.nwrite o (Printf.sprintf "  (%d, %d)\n" t nsolved)
          end
        done;
        BatIO.nwrite o "};\n";
        BatIO.nwrite o
          (Printf.sprintf "\\addlegendentry{%s}\n" reps.(0).R.config_name))
      reports;
    (* Footer. *)
    BatIO.nwrite o "\\end{axis}\n";
    BatIO.nwrite o "\\end{tikzpicture}\n";
    BatIO.nwrite o (Printf.sprintf "\\caption{%s}\n" caption);
    BatIO.nwrite o "\\end{figure}\n" in

  BatFile.with_file_out
    output_file
    (fun o ->
      BatIO.nwrite o "\\documentclass[a4paper]{article}\n";
      BatIO.nwrite o "\\usepackage[utf8]{inputenc}\n";
      BatIO.nwrite o "\\usepackage{pgfplots}\n";
      BatIO.nwrite o "\\usepackage{longtable}\n";
      BatIO.nwrite o "\\usepackage{adjustbox}\n";
      BatIO.nwrite o "\\begin{document}\n";
      write_summary_header o;
      write_summary_body o;
      write_summary_footer o;
      write_graph o label_all_groups reports;
      for gr = 0 to ngroups - 1 do
        let name = group_names.(gr) in
        let reports = List.map (fun reps -> [| reps.(gr) |]) reports in
        write_graph o name reports
      done;
      BatIO.nwrite o "\\end{document}\n")

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let output_file =
  let doc = "Write results to this file." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTPUT" ~doc)

let ngroups =
  let doc = "Number of problem groups." in
  Arg.(required & pos 1 (some int) None & info [] ~docv:"N" ~doc)

let items =
  let doc =
    "Problem group names, configuration names, directories with reports." in
  Arg.(value & pos_right 1 string [] & info [] ~docv:"ITEMS" ~doc)

let max_time =
  let doc = "Max time in graphs." in
  Arg.(value & opt int 300 &
         info ["max-time"] ~docv:"SECS" ~doc)

let label_num_problems =
  let doc = "Label of the column with problem counts." in
  Arg.(value & opt string "No. of problems" &
         info ["label-num-problems"] ~docv:"STR" ~doc)

let label_total =
  let doc = "Label of the row with totals." in
  Arg.(value & opt string "Total" &
         info ["label-total"] ~docv:"STR" ~doc)

let label_x =
  let doc = "Label of the x-axes of the graphs." in
  Arg.(value & opt string "Time (s)" &
         info ["label-x"] ~docv:"STR" ~doc)

let label_y =
  let doc = "Label of the y-axes of the graphs." in
  Arg.(value & opt string "No. of solved problems" &
         info ["label-y"] ~docv:"STR" ~doc)

let label_all_groups =
  let doc = "Caption of the graph with problems from all groups." in
  Arg.(value & opt string "All groups" &
         info ["label-all-groups"] ~docv:"STR" ~doc)

let summary_to_latex_t =
  Term.(pure summary_to_latex $ output_file $ ngroups $ items $
          max_time $
          label_num_problems $ label_total $
          label_x $ label_y $ label_all_groups)

let info =
  Term.info "summary_to_latex"

let () =
  match Term.eval (summary_to_latex_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
