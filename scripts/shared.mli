(* Copyright (c) 2013 Radek Micek *)

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


(** [run_with_limits timeout_exe max_time max_mem prog args
   new_stdin new_stdout new_stderr] executes a program in file [prog]
   with arguments [args], time limit [max_time] (in seconds)
   and memory limit [max_mem] (in kilobytes).

   [timeout_exe] is the path to a script which enforces the limits.

   Returns [(time, mem_peak, exit_status)]. [time] is the elapsed
   number of miliseconds between the invocation and the termination of [prog].
   [mem_peak] is the maximal amount of memory which was allocated
   by the program and its children.
*)
val run_with_limits :
  string -> int option -> int option ->
  string -> string array ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr ->
  int * int * Report.exit_status
