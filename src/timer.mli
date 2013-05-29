(* Copyright (c) 2013 Radek Micek *)

(** Timer. *)

(** [with_timer secs callback action] starts [action] and
   if [action] doesn't finish within [secs] seconds, [callback]
   is called in another thread. Returns the result of [action]
   and the indicator whether [callback] was run.

   [callback] must not raise exception.
*)
val with_timer : int -> (unit -> unit) -> (unit -> 'a) -> 'a * bool
