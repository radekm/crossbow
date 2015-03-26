(* Copyright (c) 2015 Radek Micek *)

let (%>) = BatPervasives.(%>)

(* ************************************************************************ *)
(* Simpler interface for SQLite *)

(** [expect func x] returns a function which checks whether
   another function named [func] returned [x].
   Raises [Failure], if it isn't the case.
*)
let expect func x x' =
  if x <> x' then failwith (Printf.sprintf "%s: Unexpected value" func)

(* Note: The name [use_stmt] starts with [use] since it only uses an
   existing statement. On the other hand we have [with_db] since
   it creates its own instance of [Sqlite3.db] (it doesn't use
   an existing instance).
*)

(** [use_stmt f stmt] invokes [f] on the given statement [stmt].
   The statement is finalized after [f] terminates.
*)
let use_stmt f stmt =
  BatPervasives.with_dispose
    ~dispose:(Sqlite3.finalize %> expect "use_stmt" Sqlite3.Rc.OK)
    f
    stmt

(** [bind stmt n data] binds the value [data] to the [n]th free variable
   of the statement [stmt].
 *)
let bind stmt n data =
  Sqlite3.bind stmt n data
  |> expect "bind" Sqlite3.Rc.OK

(** [iter_rows f stmt] applies [f] to each row produced
   by the statement [stmt]. The statement isn't finalized.
*)
let iter_rows (f : Sqlite3.Data.t list -> unit) stmt =
  let rec iter () =
    match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          f (Sqlite3.row_data stmt |> Array.to_list);
          iter ()
      | Sqlite3.Rc.DONE -> ()
      | _ -> failwith "iter_rows" in
  iter ()

(** [query_first db ~data sql] executes the first SQL statement from [sql]
   and returns rows produced by that statement.

   [data] is a list of values to be bound to the free variables
   in the SQL statement.
*)
let query_first db ?(data = []) sql =
  let rows = BatDynArray.create () in
  use_stmt
    (fun stmt ->
      List.iteri (fun i d -> bind stmt (i + 1) d) data;
      iter_rows (BatDynArray.add rows) stmt)
    (Sqlite3.prepare db sql);
  BatDynArray.to_list rows

(** [query_first db sql] executes all SQL statements from [sql]
   and returns rows produced by them.
*)
let query db sql =
  let rows = BatDynArray.create () in
  let rec exec = function
    | None -> BatDynArray.to_list rows
    | Some stmt ->
        use_stmt
          (iter_rows (BatDynArray.add rows))
          stmt;
        exec (Sqlite3.prepare_tail stmt) in
  exec (Some (Sqlite3.prepare db sql))

(** Same as [query_first] but doesn't return any rows. *)
let exec_first db ?data sql = query_first db ?data sql |> ignore

(** Same as [query] but doesn't return any rows. *)
let exec db sql = query db sql |> ignore

(** [with_db ~mode file f] executes [f] with a database [file].

   [mode] affects whether a database is created when it doesn't exist
   (by default it isn't).
*)
let with_db ?mode file f =
  let mode =
    match mode with
      | None -> Some `NO_CREATE
      | Some `Create -> None in
  BatPervasives.with_dispose
    ~dispose:(Sqlite3.db_close %> expect "with_db" true)
    (fun db ->
      exec db "PRAGMA foreign_keys = ON";
      exec db "BEGIN";
      let x = f db in
      exec db "COMMIT";
      x)
    (Sqlite3.db_open ?mode file)

module D = struct

  include Sqlite3.Data

  let of_int i = INT (Int64.of_int i)

  let to_int = function
    | INT i -> Int64.to_int i
    | _ -> failwith "to_int"

  let of_string s = TEXT s

  let to_string = function
    | TEXT s -> s
    | _ -> failwith "to_string"

  let of_sexp = Sexplib.Sexp.to_string %> of_string

  let to_sexp = to_string %> Sexplib.Sexp.of_string

  let lift_of f = function
    | None -> NULL
    | Some x -> f x

  let lift_to f = function
    | NULL -> None
    | x -> Some (f x)

  let of_opt_int = lift_of of_int

  let to_opt_int = lift_to to_int

end

(* ************************************************************************ *)

let has_schema db =
  let sql = "
    SELECT COUNT(*) FROM sqlite_master
    WHERE type = 'table' AND name IN ('config', 'result')
  " in
  let rows = query db sql in
  rows = [[D.of_int 2]]

let sql_create_schema = "
  CREATE TABLE config (
    config_name  TEXT  NOT NULL,
    solver       TEXT  NOT NULL,
    opts         TEXT  NOT NULL,
    max_time     INT   NULL,
    max_mem      INT   NULL,
    CONSTRAINT PK_config PRIMARY KEY (config_name)
  );

  CREATE TABLE result (
    config_name  TEXT  NOT NULL,
    problem      TEXT  NOT NULL,
    time         INT   NOT NULL,
    mem_peak     INT   NOT NULL,
    exit_status  TEXT  NOT NULL,
    model_size   INT   NULL,
    CONSTRAINT PK_result PRIMARY KEY (config_name, problem),
    CONSTRAINT FK_result__config
      FOREIGN KEY (config_name)
      REFERENCES config(config_name)
  )
"

let create_schema_if_not_exists db =
  if not (has_schema db) then
    exec db sql_create_schema


module Config = struct

  type t = {
    name : string;
    solver : string;
    opts : string list;
    max_time : int option;
    max_mem : int option;
  }

  let opts_of_sexp =
    let open Sexplib.Std in
    list_of_sexp string_of_sexp

  let sexp_of_opts =
    let open Sexplib.Std in
    sexp_of_list sexp_of_string

  let config_of_row row =
    match row with
      | [config_name; solver; opts; max_time; max_mem] ->
          {
            name = D.to_string config_name;
            solver = D.to_string solver;
            opts = D.to_sexp opts |> opts_of_sexp;
            max_time = D.to_opt_int max_time;
            max_mem = D.to_opt_int max_mem;
          }
      | _ -> failwith "config_of_row"

  let get' db name =
    let sql = "
      SELECT config_name, solver, opts, max_time, max_mem FROM config
      WHERE config_name = ?
    " in
    let rows =
      query_first db
        ~data:[D.of_string name]
        sql in
    match rows with
      | [row] -> Some (config_of_row row)
      | _ -> None

  let ensure report config =
    let sql = "
      INSERT INTO config (config_name, solver, opts, max_time, max_mem)
      VALUES (?, ?, ?, ?, ?)
    " in
    with_db ~mode:`Create report
      (fun db ->
        create_schema_if_not_exists db;
        match get' db config.name with
          | None ->
              exec_first db
                ~data:[
                  D.of_string config.name;
                  D.of_string config.solver;
                  D.of_sexp (sexp_of_opts config.opts);
                  D.of_opt_int config.max_time;
                  D.of_opt_int config.max_mem;
                ]
                sql
          | Some cfg when cfg <> config ->
              failwith "ensure: Different config with the same name"
          | Some _ -> ())

  let get report name =
    with_db report (fun db -> get' db name)

  let list report =
    let sql = "
      SELECT config_name, solver, opts, max_time, max_mem FROM config
      ORDER BY config_name
    " in
    with_db report
      (fun db ->
        query db sql
        |> BatList.map config_of_row)

end


module Result = struct

  type exit_status =
    | Out_of_time
    | Out_of_memory
    | Exit_code of int

  type t = {
    problem : string;
    time : int;
    mem_peak : int;
    exit_status : exit_status;
    model_size : int option;
  }

  let exit_status_of_sexp s =
    let open Sexplib.Std in
    let open Sexplib.Sexp in
    match s with
      | Atom "time" -> Out_of_time
      | Atom "memory" -> Out_of_memory
      | _ -> Exit_code (int_of_sexp s)

  let sexp_of_exit_status es =
    let open Sexplib.Std in
    let open Sexplib.Sexp in
    match es with
      | Out_of_time -> Atom "time"
      | Out_of_memory -> Atom "memory"
      | Exit_code i -> sexp_of_int i

  let check_config_exists db config_name =
    match Config.get' db config_name with
      | None -> failwith "check_config_exists"
      | Some _ -> ()

  let insert report config_name res =
    let sql = "
      INSERT INTO result
        (config_name, problem, time, mem_peak, exit_status, model_size)
      VALUES (?, ?, ?, ?, ?, ?)
    " in
    with_db report
      (fun db ->
        exec_first db
          ~data:[
            D.of_string config_name;
            D.of_string res.problem;
            D.of_int res.time;
            D.of_int res.mem_peak;
            D.of_sexp (sexp_of_exit_status res.exit_status);
            D.of_opt_int res.model_size;
          ]
          sql)

  let result_of_row row =
    match row with
      | [problem; time; mem_peak; exit_status; model_size] ->
          {
            problem = D.to_string problem;
            time = D.to_int time;
            mem_peak = D.to_int mem_peak;
            exit_status = D.to_sexp exit_status |> exit_status_of_sexp;
            model_size = D.to_opt_int model_size;
          }
      | _ -> failwith "result_of_row"

  let get' db config_name problem =
    check_config_exists db config_name;
    let sql = "
      SELECT problem, time, mem_peak, exit_status, model_size FROM result
      WHERE config_name = ? AND problem = ?
    " in
    let rows =
      query_first db
        ~data:[
          D.of_string config_name;
          D.of_string problem;
        ]
        sql in
    match rows with
      | [row] -> Some (result_of_row row)
      | _ -> None

  let get report config_name problem =
    with_db report (fun db -> get' db config_name problem)

  let list report config_name =
    let sql = "
      SELECT problem, time, mem_peak, exit_status, model_size FROM result
      WHERE config_name = ? ORDER BY problem
    " in
    with_db report
      (fun db ->
        check_config_exists db config_name;
        query_first db ~data:[D.of_string config_name] sql
        |> BatList.map result_of_row)

end
