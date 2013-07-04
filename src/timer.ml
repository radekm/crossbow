(* Copyright (c) 2013 Radek Micek *)

let get_ms () =
  let ns = Int64.to_int (Oclock.gettime Oclock.monotonic_raw) in
  ns / 1000000

let with_timer ms callback f =
  let max_ms = get_ms () + ms in
  let callback_called = ref false in
  let finished = ref false in
  let m = Mutex.create () in
  let p_read, p_write = Unix.pipe () in

  let callback_thread =
    let rec loop () =
      Mutex.lock m;
      let fin = !finished in
      Mutex.unlock m;
      if fin then
        ()
      else if get_ms () > max_ms then begin
        callback_called := true;
        callback ()
      end else begin
        (* Waiting can be interrupted by writing to a pipe. *)
        let _ = Unix.select [p_read] [] [] 0.4 in
        loop ()
      end in
    Thread.create loop () in

  let result =
    BatPervasives.with_dispose
      ~dispose:(fun () ->
        Mutex.lock m;
        finished := true;
        Mutex.unlock m;
        (* Interrupt waiting. *)
        let _ = Unix.write p_write "x" 0 1 in
        Thread.join callback_thread;
        Unix.close p_write;
        Unix.close p_read)
      f
      () in

  result, !callback_called
