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

type exit_status =
  | ES_time
  (** Program breached the time limit and it was killed. *)
  | ES_memory
  (** Program breached the memory limit and it was killed. *)
  | ES_ok of int
  (** Program exited normally. Carries the exit code. *)

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
  int * int * exit_status

(** [tptp_concat_map f base_dir in_files out_file] reads TPTP formulas and
   comments from [in_files] and files included from [in_files], transforms
   them with [f] and writes them to [out_file]. Includes with relative paths
   are resolved against [base_dir].
*)
val tptp_concat_map :
  (Tptp_ast.tptp_input -> Tptp_ast.tptp_input) ->
  string -> string list -> string -> unit
