(* Copyright (c) 2013 Radek Micek *)

open OUnit2

let run_compiler_and_save_err args out_file =
  let args = BatList.append ["ocamlfind"; "ocamlc"; "-c"] args in
  BatPervasives.with_dispose
    ~dispose:close_out
    (fun out ->
      let _ =
        Unix.create_process
          "ocamlfind"
          (Array.of_list args)
          (Unix.descr_of_in_channel stdin)
          (Unix.descr_of_out_channel stdout)
          (Unix.descr_of_out_channel out) in
      ignore (Unix.wait ()))
    (open_out out_file)

let test_syntax inc_dir earray_syntax base_name test_ctx =
  (* Contains Earray matches - must be preprocessed. *)
  let src = base_name ^ ".ml" in
  (* Earray matches were manually rewritten to ordinary matches. *)
  let src_exp = base_name ^ "_exp.ml" in
  let ast = base_name ^ ".out" in
  let ast_exp = base_name ^ "_exp.out" in
  run_compiler_and_save_err
    ["-I"; inc_dir test_ctx; "-ppx"; earray_syntax test_ctx; src; "-dsource"]
    ast;
  run_compiler_and_save_err
    ["-I"; inc_dir test_ctx; src_exp; "-dsource"]
    ast_exp;
  let lines = BatFile.lines_of ast |> BatList.of_enum in
  let lines_exp = BatFile.lines_of ast_exp |> BatList.of_enum in
  assert_equal lines_exp lines

let suite inc_dir earray_syntax =
  let test_syntax name = name >:: test_syntax inc_dir earray_syntax name in
  "suite" >:::
    [
      "Earray syntax" >:::
        [
          test_syntax "example_simple";
          test_syntax "example_guarded_match";
          test_syntax "example_nested_pattern";
          test_syntax "example_nested_match";
          test_syntax "example_or_pattern";
          test_syntax "example_nested_match_in_guard";
        ];
      Test_earray.suite;
    ]

let inc_dir =
  Conf.make_string
    "inc_dir"
    "."
    "Add directory to the list of include directories"

let earray_syntax = Conf.make_exec "earray_syntax"

let () = run_test_tt_main (suite inc_dir earray_syntax)
