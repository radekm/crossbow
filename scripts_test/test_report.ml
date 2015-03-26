(* Copyright (c) 2015 Radek Micek *)

open OUnit2

module Cfg = Report.Config
module Res = Report.Result

let assert_raises_failure f =
  try
    f ();
    assert_failure "No exception raised"
  with Failure _ -> ()

let assert_raises_sqlite3_error f =
  try
    f ();
    assert_failure "No exception raised"
  with Sqlite3.Error _ -> ()

(** Creates an empty file for report. *)
let with_report (f : string -> unit) =
  BatPervasives.with_dispose
    ~dispose:Sys.remove
    f
    (Filename.temp_file "report" "")

let config = {
  Cfg.name = "aa";
  Cfg.solver = "solver";
  Cfg.opts = ["\"\\\""; "--opt"];
  Cfg.max_time = None;
  Cfg.max_mem = Some (11 * 1000 * 1000);
}

let config' = { config with Cfg.max_time = Some 60 }

let config2 = {
  Cfg.name = "bb";
  Cfg.solver = "solver";
  Cfg.opts = ["(opt)"];
  Cfg.max_time = Some 120;
  Cfg.max_mem = Some (9 * 1000 * 1000);
}

let test_config test_ctx =
  with_report (fun r ->
    (* Create report (database + schema) and insert configuration. *)
    Cfg.ensure r config;
    (* No-op - configuration is already present. *)
    Cfg.ensure r config;
    (* Different configuration with the same name. *)
    assert_raises_failure (fun () -> Cfg.ensure r config');
    (* Insert configuration. *)
    Cfg.ensure r config2;

    (* Cfg.get *)
    assert_equal (Some config) (Cfg.get r config.Cfg.name);
    assert_equal (Some config2) (Cfg.get r config2.Cfg.name);
    assert_equal None (Cfg.get r "foo");

    (* Cfg.list *)
    assert_equal [config; config2] (Cfg.list r))

let result_monoid = {
  Res.problem = "monoid";
  Res.time = 5;
  Res.mem_peak = 5000;
  Res.exit_status = Res.Exit_code 0;
  Res.model_size = Some 5;
}

let result_monoid2 = {
  Res.problem = "monoid";
  Res.time = 25;
  Res.mem_peak = 8000;
  Res.exit_status = Res.Exit_code 0;
  Res.model_size = None;
}

let result_loop = {
  Res.problem = "loop";
  Res.time = 120;
  Res.mem_peak = 9000;
  Res.exit_status = Res.Out_of_time;
  Res.model_size = None;
}

let test_result test_ctx =
  with_report (fun r ->
    Cfg.ensure r config;
    Cfg.ensure r config2;

    Res.insert r config.Cfg.name result_monoid;
    Res.insert r config.Cfg.name result_loop;

    (* Insert existing result. *)
    assert_raises_failure (fun () ->
      Res.insert r config.Cfg.name result_monoid);

    Res.insert r config2.Cfg.name result_monoid2;

    (* Res.get *)
    assert_equal
      (Some result_loop)
      (Res.get r config.Cfg.name result_loop.Res.problem);
    assert_equal
      None
      (Res.get r config2.Cfg.name result_loop.Res.problem);

    (* Res.list *)
    assert_equal
      [result_loop; result_monoid]
      (Res.list r config.Cfg.name);
    assert_equal
      [result_monoid2]
      (Res.list r config2.Cfg.name))

let test_result_functions_need_config_in_report test_ctx =
  with_report (fun r ->
    (* Create schema but don't insert config. *)
    Cfg.ensure r config2;

    (* All functions from module Res take configuration name
       and raise Failure when no configuration with that name exists.
    *)
    assert_raises_failure (fun () ->
      Res.insert r config.Cfg.name result_monoid);
    assert_raises_failure (fun () ->
      Res.get r config.Cfg.name result_monoid.Res.problem);
    assert_raises_failure (fun () ->
      Res.list r config.Cfg.name))

let test_report_created_only_by_ensure test_ctx =
  with_report (fun r ->
    Sys.remove r;

    (* Cfg.get, Cfg.list, Res.insert, Res.get, Res.list don't create
       the report but instead they raise Sqlite3.Error
       when the report doesn't exist.
    *)
    assert_raises_sqlite3_error (fun () -> Cfg.get r "foo");
    assert_raises_sqlite3_error (fun () -> Cfg.list r);
    assert_raises_sqlite3_error (fun () ->
      Res.insert r config.Cfg.name result_monoid);
    assert_raises_sqlite3_error (fun () ->
      Res.get r config.Cfg.name result_monoid.Res.problem);
    assert_raises_sqlite3_error (fun () -> Res.list r config.Cfg.name);
    assert_equal false (Sys.file_exists r);

    (* Cfg.ensure creates the report when it doesn't exist. *)
    Cfg.ensure r config';
    assert_equal true (Sys.file_exists r))

let suite =
  "Report suite" >:::
    [
      "Config" >:: test_config;
      "Result" >:: test_result;
      "functions from Result raise if configuration doesn't exist" >::
        test_result_functions_need_config_in_report;
      "only Config.ensure creates report" >::
        test_report_created_only_by_ensure;
    ]
