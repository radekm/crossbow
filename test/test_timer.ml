(* Copyright (c) 2013 Radek Micek *)

open OUnit

let test_with_timer_no_callback () =
  let res, called =
    Timer.with_timer (3600 * 1000)
      (fun () -> ())
      (fun () -> Thread.delay 1.; 3) in
  assert_equal 3 res;
  assert_equal false called

let test_with_timer_no_callback_exn () =
  assert_raises
    (Failure "x")
    (fun () ->
      ignore (Timer.with_timer 5000
                (fun () -> ())
                (fun () -> failwith "x")))

let test_with_timer_callback () =
  let res, called =
    Timer.with_timer 1000
      (fun () -> ())
      (fun () -> Thread.delay 1.5; "res") in
  assert_equal "res" res;
  assert_equal true called

let test_with_timer_callback_exn () =
  assert_raises
    (Failure "x")
    (fun () ->
      ignore (Timer.with_timer 0
                (fun () -> ())
                (fun () -> Thread.delay 1.; failwith "x")))

let suite =
  "Timer suite" >:::
    [
      "with_timer - no callback" >:: test_with_timer_no_callback;
      "with_timer - no callback, exn" >:: test_with_timer_no_callback_exn;
      "with_timer - callback" >:: test_with_timer_callback;
      "with_timer - callback, exn" >:: test_with_timer_callback_exn;
    ]
