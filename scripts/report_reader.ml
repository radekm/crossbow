(* Copyright (c) 2013 Radek Micek *)

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

(* Assumes that model was not found when the exit code <> 0. *)
let model_found res =
  res.exit_status = Exit_code 0 &&
  res.model_size <> None

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
