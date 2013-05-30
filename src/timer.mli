(* Copyright (c) 2013 Radek Micek *)

(** Timer. *)

(** Returns the number of miliseconds from some fixed epoch. *)
val get_ms : unit -> int

(** [with_timer ms callback action] starts [action] and
   if [action] doesn't finish within [ms] miliseconds, [callback]
   is called in another thread. Returns the result of [action]
   and the indicator whether [callback] was run.

   [callback] must not raise exception.
*)
val with_timer : int -> (unit -> unit) -> (unit -> 'a) -> 'a * bool
