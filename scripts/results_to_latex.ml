(* Copyright (c) 2015 Radek Micek *)

let (%>) = BatPervasives.(%>)

let sprintf = Printf.sprintf
let fprintf = Printf.fprintf

let uncons err = function
  | [] -> failwith err
  | x :: xs -> x, xs

(* ************************************************************************ *)
(* LaTeX *)

module Latex = struct

  let unimp = sprintf "{\\footnotesize %s}"

  let imp = sprintf "\\textbf{%s}"

  let unimp_int = string_of_int %> unimp

  let imp_int = string_of_int %> imp

  let pdf_tooltip ~detail s = sprintf "\\pdftooltip{%s}{%s}" s detail

  module Doc = struct

    let header o =
      fprintf o "\\documentclass[a4paper]{article}\n";
      fprintf o "\\usepackage[utf8]{inputenc}\n";
      fprintf o "\\usepackage{pdfcomment}\n";
      fprintf o "\\usepackage{pgfplots}\n";
      fprintf o "\\pgfplotsset{compat=1.8}\n";
      fprintf o "\\usepackage{longtable}\n";
      fprintf o "\\usepackage{adjustbox}\n";
      fprintf o "\\begin{document}\n"

    let footer o =
      fprintf o "\\end{document}\n"

  end

  module Table = struct

    let hline o = fprintf o "\\hline\n"

    let header o cells =
      let c, cs = uncons "table_header" cells in
      fprintf o "\\begin{longtable}{l";
      List.iter (fun _ -> fprintf o "|c") cs;
      fprintf o "}\n";
      let title s =
        if s <> "" then
          fprintf o "\\multicolumn{1}{c}{\\adjustbox{angle=90}{%s}}"
            (unimp s) in
      title c;
      List.iter
        (fun c ->
          fprintf o " & ";
          title c)
        cs;
      fprintf o "\\\\\n";
      hline o;
      fprintf o "\\endhead\n"

    let row o cells =
      let c, cs = uncons "table_row" cells in
      fprintf o "%s" c;
      List.iter (fprintf o " & %s") cs;
      fprintf o "\\\\\n";
      hline o

    let footer o =
      fprintf o "\\end{longtable}\n"

  end

end

(* ************************************************************************ *)

module Cfg = Report.Config
module Res = Report.Result

(** A problem is considered solved iff the model size is known
   and exit code is zero.

   Note: A problem is considered solved even if the solver breached
   the time limit or the memory limit.
*)
let is_solved r =
  r.Res.exit_status = Res.Exit_code 0 &&
  r.Res.model_size <> None

type cfg_name = string

type grp_name = string

(** Results for single group or combined results for all groups. *)
module Res_one = struct

  type t = (cfg_name * Res.t list) list

end

type res_by_grp = (grp_name * Res.t list) list

(** Results for all groups. *)
module Res_all : sig

  type t = (cfg_name * res_by_grp) list

  val combine_results_for_all_groups : t -> Res_one.t

  val iter_by_group : (grp_name * Res_one.t -> unit) -> t -> unit

end = struct

  type t = (cfg_name * res_by_grp) list

  let count_groups (res_all : t) =
    res_all |> List.hd |> snd |> List.length

  let combine_results_for_all_groups (res_all : t) : Res_one.t =
    let combine (res_by_grp : res_by_grp) : Res.t list =
      res_by_grp
      |> BatList.map snd
      |> BatList.concat in
    BatList.map
      (fun (cfg_name, res_by_grp) -> cfg_name, combine res_by_grp)
      res_all

  let get_results_for_group
      (res_all : t)
      gr
      : grp_name * ((cfg_name * Res.t list) list) =
    let group_name = List.nth (res_all |> List.hd |> snd) gr |> fst in
    let results =
      BatList.map
        (fun (cfg_name, res_by_grp) ->
          cfg_name, List.nth res_by_grp gr |> snd)
        res_all in
    group_name, results

  let iter_by_group (f : grp_name * Res_one.t -> unit) (res_all : t) =
    let ngroups = count_groups res_all in
    for gr = 0 to ngroups - 1 do
      get_results_for_group res_all gr |> f
    done

end

(*

Recommended naming of variables:

 Type           Name
----------------------------------------------------------------------
 Res.t       |  res
 Res.t list  |  results
 res_by_grp  |  res_by_grp
 Res_one.t   |  res_one (results for single group) or
             |  res_total (results for all groups combined together)
 Res_all.t   |  res_all
----------------------------------------------------------------------

*)

(* ************************************************************************ *)

(** Generates a plot which shows how the number of solved problems depends
   on time. Time is on the x-axis and the number of solved problems
   is on the y-axis.

   The point [(x, y)] of the graph means that [y] problems can be solved
   in [x] seconds or less.

   Expects that the problems are same for all configurations
*)
let plot
    o
    ~label_x
    ~label_y
    ~caption
    max_time
    (res_one : Res_one.t) =

  (* Header. *)
  fprintf o "\n\n\n\n";
  fprintf o "\\begin{figure}\n";
  fprintf o "\\begin{tikzpicture}\n";
  fprintf o "\\begin{axis}[";
  fprintf o "%s"
    (String.concat ","
       [
         "cycle list name=exotic";
         "width=12cm";
         "height=10cm";
         "legend style={at={(0.5,1.03)},anchor=south}";
         "legend columns=2";
         "xlabel=" ^ label_x;
         "ylabel=" ^ label_y;
       ]);
  fprintf o "]\n";

  let graph_step = 10 in
  let count_solved_problems results time_secs =
    let is_solved_in_time r = is_solved r && r.Res.time < time_secs in
    results
    |> List.filter is_solved_in_time
    |> List.length in

  (* Plot for each configuration. *)
  List.iter
    (fun (cfg_name, results) ->
      fprintf o "\\addplot coordinates {\n";
      for t = 1 to max_time do
        if t = 1 || t = max_time || t mod graph_step = 0 then begin
          let nsolved = count_solved_problems results t in
          fprintf o "  (%d, %d)\n" t nsolved
        end
      done;
      fprintf o "};\n";
      fprintf o "\\addlegendentry{%s}\n" cfg_name)
    res_one;

  (* Footer. *)
  fprintf o "\\end{axis}\n";
  fprintf o "\\end{tikzpicture}\n";
  fprintf o "\\caption{%s}\n" caption;
  fprintf o "\\end{figure}\n"

let plots
    o
    ~label_x
    ~label_y
    ~caption_all_groups
    ~caption_one_group
    max_time
    (res_all : Res_all.t) =

  plot o
    ~label_x
    ~label_y
    ~caption:caption_all_groups
    max_time
    (Res_all.combine_results_for_all_groups res_all);

  Res_all.iter_by_group
    (fun (grp_name, res_one) ->
      plot o
        ~label_x
        ~label_y
        ~caption:(caption_one_group grp_name)
        max_time
        res_one)
    res_all

(** Expects that the problems are same for all configurations
   and have same order.

Generates a table like this:

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
let table_times
    o
    ~label_out_of_time
    ~label_out_of_memory
    ~label_error
    (res_one : Res_one.t) =

  let header =
    "" :: BatList.map (fun (cfg_name, _) -> cfg_name) res_one in
  Latex.Table.header o header;

  let nproblems = res_one |> List.hd |> snd |> List.length in
  let show_result best_time r =
    let detail =
      let time = sprintf "time (s): %d" r.Res.time in
      let mem_peak = sprintf "memory peak (MiB): %d" r.Res.mem_peak in
      let exit_status =
        sprintf "exit status: %s"
          (match r.Res.exit_status with
             | Res.Out_of_time -> "out of time"
             | Res.Out_of_memory -> "out of memory"
             | Res.Exit_code i -> string_of_int i) in
      let model_size =
        sprintf "model size: %s"
          (match r.Res.model_size with
             | Some i -> string_of_int i
             | None -> "no model") in
      String.concat "; " [time; mem_peak; exit_status; model_size] in
    let basic_info =
      match r.Res.exit_status with
        | Res.Out_of_time -> Latex.unimp label_out_of_time
        | Res.Out_of_memory -> Latex.unimp label_out_of_memory
        | Res.Exit_code 0 ->
            if r.Res.time = best_time
            then Latex.imp_int r.Res.time
            else Latex.unimp_int r.Res.time
        | Res.Exit_code _ -> Latex.unimp label_error in
    Latex.pdf_tooltip ~detail basic_info in
  let get_results_for_problem pr =
    BatList.map
      (fun (_, results) -> List.nth results pr)
      res_one in
  for pr = 0 to nproblems - 1 do
    let res_for_prob = get_results_for_problem pr in
    let prob_name =
      let chop_ext name =
        (* Filename.chop_extension raises Invalid_argument
           if the name doesn't contain an extension.
        *)
        try Filename.chop_extension name
        with Invalid_argument _ -> name in
      res_for_prob
      |> List.hd
      |> (fun r -> r.Res.problem)
      |> Filename.basename
      |> chop_ext in
    let best_time =
      res_for_prob
      |> BatList.map (fun r -> if is_solved r then r.Res.time else max_int)
      |> BatList.min in
    let row =
      Latex.unimp prob_name ::
      BatList.map (show_result best_time) res_for_prob in
    Latex.Table.row o row;

    (* Check that all models have equal size. *)
    let model_sizes =
      res_for_prob |> BatList.map (fun r -> r.Res.model_size) in
    if
      model_sizes
      |> BatList.filter_map (fun x -> x)
      (* Model size -1 means that a model was found
         but its size was not determined.
      *)
      |> BatList.filter (fun x -> x <> -1)
      |> BatList.unique
      (* The number of different model sizes should be 0 (no configuration
         determined model size) or 1 (single model size was determined).
      *)
      |> List.length > 1
    then
      let sizes_str =
        BatList.map
          (function
            | None -> "?"
            | Some size -> string_of_int size)
          model_sizes
        |> BatString.concat ", " in
      fprintf stderr "Warning: different model sizes %s,\n  problem %s\n"
        sizes_str prob_name
  done;

  Latex.Table.footer o

let tables_times
    o
    ~label_out_of_time
    ~label_out_of_memory
    ~label_error =

  Res_all.iter_by_group
    (fun (_, res_one) ->
      table_times o
        ~label_out_of_time
        ~label_out_of_memory
        ~label_error
        res_one)

(** Expects that the problems are same for all configurations.

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
let table_counts
    o
    ~label_num_problems
    ~label_total
    (res_all : Res_all.t) =

  let header =
    "" :: label_num_problems ::
    BatList.map (fun (cfg_name, _) -> cfg_name) res_all in
  Latex.Table.header o header;

  (* Print numbers of solved problems for each group. *)
  let count_solved_problems results =
    results |> BatList.filter is_solved |> List.length in
  Res_all.iter_by_group
    (fun (grp_name, res_one) ->
      let nproblems = res_one |> List.hd |> snd |> List.length in
      let nsolved =
        BatList.map
          (fun (_, results) -> count_solved_problems results)
          res_one in
      let max_solved = BatList.max nsolved in
      let row =
        Latex.unimp grp_name ::
        Latex.unimp_int nproblems ::
        BatList.map
          (fun n ->
            if n = max_solved
            then Latex.imp_int n
            else Latex.unimp_int n)
          nsolved in
      Latex.Table.row o row)
    res_all;

  (* Print totals - i.e. numbers of solved problems in all groups. *)
  let res_total = Res_all.combine_results_for_all_groups res_all in
  let nproblems_total = res_total |> List.hd |> snd |> List.length in
  let nsolved_total =
    BatList.map
      (fun (_, results) -> count_solved_problems results)
      res_total in
  let max_solved_total = BatList.max nsolved_total in
  let row =
    Latex.unimp label_total ::
    Latex.unimp_int nproblems_total ::
    List.map
      (fun n ->
        if n = max_solved_total
        then Latex.imp_int n
        else Latex.unimp_int n)
      nsolved_total in
  Latex.Table.hline o;
  Latex.Table.row o row;

  Latex.Table.footer o

(** Expects that the problems are same for all configurations
   and have same order.

Generates a table like this:

        | Config    Config    Config
        | name 1    name 2    name 3
---------------------------------------
 Config |         |    –    |    –
 name 1 |         |         |
---------------------------------------
 Config |    ●    |         |    ●
 name 2 |         |         |
---------------------------------------
 Config |    ○    |    –    |
 name 3 |         |         |
---------------------------------------

where the cell (i, j) (i-th row, j-th column) represents a result
of a sign test with

  H0: i-th and j-th configurations are equally fast
  HA: i-th configuration is faster than j-th configuration

● is used when p-value < 0.01.
○ is used when p-value < 0.05.
– is used when p-value >= 0.05.

So the second row of the table above shows that the configuration 2
is faster than the other two configurations with the significance level 0.01.
*)
let table_hypothesis_tests o (res_all : Res_all.t) =
  let header =
    "" :: BatList.map (fun (cfg_name, _) -> cfg_name) res_all in
  Latex.Table.header o header;

  let compute_pvalue results results2 =
    (* Negative number means that the first configuration is faster,
       positive number means that the second configuration is faster
       and zero means that both configurations are equally fast
       or that it is not known which is faster.
     *)
    let signs =
      BatList.map2
        (fun r r2 ->
          let t, t2 = r.Res.time, r2.Res.time in
          (* Both configurations succeeded. *)
          if is_solved r && is_solved r2 then
            t - t2
          (* Only the first configuration succeeded. *)
          else if is_solved r then
            (* Since the second configuration failed it isn't known how
               fast it is. It's only known that it uses t2 or more seconds.
               So the first configuration can be proclaimed faster
               than the second only when it uses less than t2 seconds
               (otherwise it can be as fast as the second or even slower).
            *)
            if t < t2 then -1 else 0
          (* Only the second configuration succeeded. *)
          else if is_solved r2 then
            if t > t2 then 1 else 0
          (* No configuration succeeded. *)
          else 0)
      results
      results2 in
    let nneg = signs |> BatList.filter (fun s -> s < 0) |> List.length in
    let nzero = signs |> BatList.filter (fun s -> s = 0) |> List.length in
    let npos = signs |> BatList.filter (fun s -> s > 0) |> List.length in
    nneg, nzero, npos,
    Sign_test.test ~nneg ~nzero ~npos ~ha:Sign_test.Lower in
  let show_pvalue (nneg, nzero, npos, pval) =
    let detail =
      sprintf "neg: %d; zero: %d; pos: %d; p-value %.5f\\%%"
        nneg nzero npos (pval *. 100.) in
    let basic_info =
      if pval < 0.01 then
        "$\\bullet$"
      else if pval < 0.05 then
        "$\\circ$"
      else
        "--" in
    Latex.pdf_tooltip ~detail (Latex.unimp basic_info) in
  let res_total = Res_all.combine_results_for_all_groups res_all in
  List.iteri
    (fun i (cfg_name, results) ->
      let row =
        Latex.unimp cfg_name ::
        BatList.mapi
          (fun j (_, results2) ->
            if i = j
            then ""
            else show_pvalue (compute_pvalue results results2))
          res_total in
      (* Print i-th row. *)
      Latex.Table.row o row)
    res_total;

  Latex.Table.footer o

(* ************************************************************************ *)
(* Reading of input *)

module Input : sig

  val read_results :
    string -> string list -> grp_name list -> string list -> Res_all.t

end = struct

  type problem = string

  type probs_by_grp = (grp_name * problem list) list

  (** Reads a problem list for each group and returns the groups
     with associated problems. Groups without any problems are not returned.

     The number of groups must be equal to the number of problem lists.
  *)
  let read_problems
      (group_names : grp_name list)
      problem_lists
      : probs_by_grp  =

    let rec read group_names problem_lists acc =
      match group_names, problem_lists with
        | [], [] -> List.rev acc
        | [], p :: _ ->
            failwith (sprintf "Missing group name for %s" p)
        | g :: _, [] ->
            failwith (sprintf "Missing problem list for %s" g)
        | g :: gs, p :: ps ->
           let probs =
             BatFile.lines_of p
             |> BatEnum.filter (fun p -> BatString.trim p <> "")
             |> BatList.of_enum in
           match probs with
             | [] -> read gs ps acc
             | _ -> read gs ps ((g, probs) :: acc) in
    read group_names problem_lists []

  (** Reads results for the given configuration and the given problems. *)
  let read_results_for_config
      report
      config_name
      (problems_by_groups : probs_by_grp)
      : res_by_grp =

    (* Hash table which maps problems to their results. *)
    let results =
      Res.list report config_name
      |> BatList.enum
      |> BatEnum.map (fun res -> res.Res.problem, res)
      |> BatHashtbl.of_enum in
    let results_for_group (group_name, problems) =
      (* Finds result of the problem p in the hash table and removes it. *)
      let result_for_problem p =
        try
          let result = Hashtbl.find results p in
          Hashtbl.remove results p;
          result
        with Not_found ->
          let msg =
            sprintf "No result for problem %s in configuration %s"
              p
              config_name in
          failwith msg in
      let results =
        problems
        |> BatList.map result_for_problem in
      (group_name, results) in
    problems_by_groups
    |> BatList.map results_for_group

  (** Ensures that all configurations have equal [max_time] and
     all have equal [max_mem].
  *)
  let check_compatibility_of_configs report config_names =
    let configs =
      List.map
        (fun cfg_name ->
          match Cfg.get report cfg_name with
            | None -> failwith (sprintf "Missing configuration %s" cfg_name)
            | Some config -> config)
        config_names in
    match configs with
      | []
      | [_] -> ()
      | cfg :: cfgs ->
          List.iter
            (fun c ->
              if
                cfg.Cfg.max_time <> c.Cfg.max_time ||
                cfg.Cfg.max_mem <> c.Cfg.max_mem
              then
                failwith
                  (sprintf
                    "Configurations %s and %s are not compatible"
                    cfg.Cfg.name
                    c.Cfg.name))
            cfgs

  let read_results report config_names group_names problem_lists =
    check_compatibility_of_configs report config_names;
    let problems = read_problems group_names problem_lists in
    List.map
      (fun cfg_name ->
        cfg_name, read_results_for_config report cfg_name problems)
      config_names

end

(* ************************************************************************ *)
(* Main *)

type lang =
  | En
  | Cs

type lang_data = {
  label_out_of_time : string;
  label_out_of_memory : string;
  label_error : string;
  label_num_problems : string;
  label_total : string;
  label_x : string;
  label_y : string;
  caption_all_groups : string;
  caption_one_group : string -> string;
}

let lang_en = {
  label_out_of_time = "t";
  label_out_of_memory = "m";
  label_error = "x";
  label_num_problems = "No. of problems";
  label_total = "Total";
  label_x = "Time (s)";
  label_y = "No. of solved problems";
  caption_all_groups = "All problem groups together";
  caption_one_group = sprintf "Problem group %s";
}

let lang_cs = {
  label_out_of_time = "č";
  label_out_of_memory = "p";
  label_error = "x";
  label_num_problems = "Počet problémů";
  label_total = "Celkem";
  label_x = "Čas (s)";
  label_y = "Počet vyřešených problémů";
  caption_all_groups = "Všechny skupiny problémů dohromady";
  caption_one_group = sprintf "Skupina problémů %s";
}

let get_lang_data = function
  | En -> lang_en
  | Cs -> lang_cs

type output_type =
  | Times
  | Counts
  | Plots
  | Hypothesis_tests

let main
    output_type
    (* Input. *)
    report config_names group_names problem_lists
    (* Optional settings. *)
    output_file lang max_time =

  let lang = get_lang_data lang in
  let res_all =
    Input.read_results report config_names group_names problem_lists in
  let output, close_output =
    match output_file with
      | None -> stdout, (fun _ -> ())
      | Some file -> open_out file, close_out in
  BatPervasives.with_dispose
    ~dispose:close_output
    (fun o ->
      Latex.Doc.header o;
      begin match output_type with
        | Times ->
            tables_times
              o
              ~label_out_of_time:lang.label_out_of_time
              ~label_out_of_memory:lang.label_out_of_memory
              ~label_error:lang.label_error
              res_all
        | Counts ->
            table_counts o
              ~label_num_problems:lang.label_num_problems
              ~label_total:lang.label_total
              res_all
        | Plots ->
            plots
              o
              ~label_x:lang.label_x
              ~label_y:lang.label_y
              ~caption_all_groups:lang.caption_all_groups
              ~caption_one_group:lang.caption_one_group
              max_time
              res_all
        | Hypothesis_tests -> table_hypothesis_tests o res_all
      end;
      Latex.Doc.footer o)
    output

module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

let output_type =
  let doc =
    "Which output is desired. " ^
    "One of: times, counts, plots, hypothesis_tests" in
  let types = [
    "times", Times;
    "counts", Counts;
    "plots", Plots;
    "hypothesis_tests", Hypothesis_tests;
  ] in
  Arg.(required & pos 0 (some & enum types) None & info []
         ~docv:"OUTPUT-TYPE" ~doc)

let report =
  let doc = "Report with the results." in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"REPORT" ~doc)

let config =
  let doc =
    "Configuration name." in
  Arg.(value & opt_all string [] & info ["config"] ~docv:"CONFIG" ~doc)

let group =
  let doc =
    "Group name." in
  Arg.(value & opt_all string [] & info ["group"] ~docv:"GROUP" ~doc)

let problems =
  let doc =
    "File with a list of problems in the corresponding group." in
  Arg.(value & opt_all file [] & info ["problems"] ~docv:"PROBLEMS" ~doc)

let output_file =
  let doc = "Write results to this file." in
  Arg.(value & opt (some string) None & info ["output-file"]
         ~docv:"OUTPUT-FILE" ~doc)

let lang =
  let doc = "Language of the output. One of: en, cs" in
  let langs = [
    "en", En;
    "cs", Cs;
  ] in
  Arg.(value & opt (enum langs) En & info ["lang"]
         ~docv:"LANG" ~doc)

let max_time =
  let doc = "Max time in plots (in seconds)." in
  Arg.(value & opt int 300 &
         info ["max-time"] ~docv:"MAX-TIME" ~doc)

let main_t =
  Term.(pure main $ output_type $ report $
          config $ group $ problems $
          output_file $ lang $ max_time)

let info =
  Term.info "results_to_latex"

let () =
  match Term.eval (main_t, info) with
    | `Error _ -> exit 2
    | _ -> exit 0
