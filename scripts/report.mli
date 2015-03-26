(* Copyright (c) 2015 Radek Micek *)

module Config : sig

  type t = {
    name : string;
    solver : string;
    opts : string list;
    (** Options given to the solver. *)
    max_time : int option;
    max_mem : int option;
  }

  (** [ensure report config] ensures that the configuration [config]
     is in the report [report].

     The report [report] will be created if it doesn't exist or
     if it's an empty file.

     Raises [Failure] if [report] contains a different configuration
     with the same name as [config].
  *)
  val ensure : string -> t -> unit

  (** [get report config_name] returns a configuration named [config_name].

     Raises [Sqlite3.Error] if the report doesn't exist.
  *)
  val get : string -> string -> t option

  (** [list report] returns all configurations from [report]
     sorted by the name.

     Raises [Sqlite3.Error] if the report doesn't exist.
  *)
  val list : string -> t list

end


module Result : sig

  type exit_status =
    | Out_of_time
    (** Program breached the time limit and it was killed. *)
    | Out_of_memory
    (** Program breached the memory limit and it was killed. *)
    | Exit_code of int
    (** Program didn't breach any limit. *)

  type t = {
    problem : string;
    time : int;
    mem_peak : int;
    exit_status : exit_status;
    model_size : int option;
  }

  (** [insert report config_name res] inserts the result [res]
     to the configuration named [config_name].

     Raises [Sqlite3.Error] if the report doesn't exist.
     Raises [Failure] if the configuration doesn't exist.
  *)
  val insert : string -> string -> t -> unit

  (** [get report config_name prob] returns a result for the problem [prob]
     from the configuration named [config_name].

     Raises [Sqlite3.Error] if the report doesn't exist.
     Raises [Failure] if the configuration doesn't exist.
  *)
  val get : string -> string -> string -> t option

  (** [list report config_name] returns results sorted by the problem
     from the configuration named [config_name].

     Raises [Sqlite3.Error] if the report doesn't exist.
     Raises [Failure] if the configuration doesn't exist.
  *)
  val list : string -> string -> t list

end
