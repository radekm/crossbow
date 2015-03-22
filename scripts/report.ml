(* Copyright (c) 2015 Radek Micek *)

type exit_status =
  (** Program breached the time limit and it was killed. *)
  | Out_of_time
  (** Program breached the memory limit and it was killed. *)
  | Out_of_memory
  (** Program didn't breach any limit. *)
  | Exit_code of int

type result = {
  problem : string;
  time : int;
  mem_peak : int;
  exit_status : exit_status;
  model_size : int option;
}

type t = {
  config_name : string;
  solver : string;
  opts : string list;
  max_time : int option;
  max_mem : int option;
  results : result list;
}

let to_json r =
  let result_to_json res =
    let exit_status =
      match res.exit_status with
        | Out_of_time -> `String "time"
        | Out_of_memory -> `String "memory"
        | Exit_code i -> `Int i in
    let model_size =
      BatOption.map_default (fun i -> `Int i) `Null res.model_size in
    `Assoc
      [
        "problem", `String res.problem;
        "time", `Int res.time;
        "mem_peak", `Int res.mem_peak;
        "exit_status", exit_status;
        "model_size", model_size;
      ] in
  let max_time =
    BatOption.map_default (fun max_time -> `Int max_time) `Null r.max_time in
  let max_mem =
    BatOption.map_default (fun max_mem -> `Int max_mem) `Null r.max_mem in
  let results = `List (r.results |> BatList.map result_to_json) in
  `Assoc
    [
      "config_name", `String r.config_name;
      "solver", `String r.solver;
      "opts", `List (BatList.map (fun opt -> `String opt) r.opts);
      "max_time", max_time;
      "max_mem", max_mem;
      "results", results;
    ]

let of_json json =
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
        let get name = List.assoc name fields in
        {
          problem = read_string (get "problem");
          time = read_int (get "time");
          mem_peak = read_int (get "mem_peak");
          exit_status = read_exit_status (get "exit_status");
          model_size = read_int_or_null (get "model_size");
        }
    | _ -> failwith "read_result" in
  let read_results = function
    | `List results -> List.map read_result results
    | _ -> failwith "read_results" in
  match json with
    | `Assoc fields ->
        let get name = List.assoc name fields in
        {
          config_name = read_string (get "config_name");
          solver = read_string (get "solver");
          opts = read_opts (List.assoc "opts" fields);
          max_time = read_int_or_null (get "max_time");
          max_mem = read_int_or_null (get "max_mem");
          results = read_results (get "results");
        }
    | _ -> failwith "report_of_json"

let read file =
  Yojson.Safe.from_file file
  |> of_json

let write file rep =
  let json = to_json rep in
  BatFile.with_file_out file
    (fun out -> BatIO.nwrite out (Yojson.Safe.pretty_to_string json))

let default_filename = "__REPORT__"
