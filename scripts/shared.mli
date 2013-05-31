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

(** [run_with_lim_mem timeout_exe max_mem prog args new_stdin new_stdout
   new_stderr] executes a program in file [prog] with arguments [args]
   and memory limit [max_mem] (in kilobytes).

   [timeout_exe] is the path to a script which enforces the memory limit.

   Returns [(time, mem_peak, exit_code)]. [time] is the elapsed
   number of miliseconds between the invocation and the termination of [prog].
   [mem_peak] is the maximal amount of memory which was allocated
   by the program and its children. [exit_code] is [None] when the program
   breached the memory limit [max_mem], otherwise it contains the exit code.
*)
val run_with_lim_mem :
  string -> int ->
  string -> string array ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr ->
  int * int * int option
