(* Copyright (c) 2013, 2015 Radek Micek *)

(** Reads a list of problems from file.

   Checks that all files with problems exist and have unique names.
*)
val read_problems_of_file : string -> string list

(** Extracts file name from the given path. *)
val file_name : string -> string

(** [file_in_dir dir file] returns path to the file [file]
   in the directory [dir].
*)
val file_in_dir : string -> string -> string

(** [file_in_program_dir file] returns path to the file [file]
   in the directory containing the executable which is currently running.
*)
val file_in_program_dir : string -> string

(** [run_with_limits cgroup max_time max_mem prog args
   new_stdin new_stdout new_stderr] executes a program in file [prog]
   with arguments [args], CPU time limit [max_time] (in seconds)
   and memory limit [max_mem] (in mebibytes).

   [cgroup] is a relative path to control groups in [cpuacct] controller
   and [memory] controller.

   Returns [(time, mem_peak, exit_status)].
   [time] is the number of CPU seconds consumed by [prog].
   [mem_peak] is the maximal amount of memory (in mebibytes)
   which was allocated by the program and its children.
*)
val run_with_limits :
  string -> int option -> int option ->
  string -> string array ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr ->
  int * int * Report.Result.exit_status
