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

val milisecs_to_secs : int -> int

val model_found : result -> bool

val report_from_file : string -> string -> report

val report_file_name : string
