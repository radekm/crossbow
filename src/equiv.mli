(* Copyright (c) 2013 Radek Micek *)

(** Union-find algorithm. *)

(** Equivalence. *)
type t

(** Item in an equivalence. *)
type item

(** Id of an equivalence block. *)
type block_id = int

(** Creates a new equivalence over the empty set. *)
val create : unit -> t

(** Adds a new block with a new item to the equivalence.
   The new item is returned.
*)
val add_item : t -> item

(** Merges the blocks which contain the given items. *)
val union : t -> item -> item -> unit

(** Returns the id of the block which contains the given item. *)
val find : t -> item -> block_id
