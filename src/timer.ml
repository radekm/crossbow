(* Copyright (c) 2013 Radek Micek *)

let with_timer secs callback f =
  let ns = Int64.of_int (secs * 1000 * 1000 * 1000) in
  let max_ns = BatInt64.(Oclock.gettime Oclock.monotonic_raw + ns) in
  let callback_called = ref false in
  let finished = ref false in
  let m = Mutex.create () in

  let callback_thread =
    let rec loop () =
      Mutex.lock m;
      let fin = !finished in
      Mutex.unlock m;
      if fin then
        ()
      else if
        BatInt64.(Oclock.gettime Oclock.monotonic_raw > max_ns)
      then begin
        callback_called := true;
        callback ()
      end else begin
        Thread.delay 0.4;
        loop ()
      end in
    Thread.create loop () in

  let result =
    BatPervasives.with_dispose
      ~dispose:(fun () ->
        Mutex.lock m;
        finished := true;
        Mutex.unlock m;
        Thread.join callback_thread)
      f
      () in

  result, !callback_called
