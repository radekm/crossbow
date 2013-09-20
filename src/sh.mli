(* Copyright (c) 2013 Radek Micek *)

(** Shared. *)

type sign =
  | Pos
  | Neg

val neg : sign -> sign

type lbool =
  | Ltrue
  | Lfalse
  | Lundef

module IntSet : BatSet.S with type elt = int
module IntMap : BatMap.S with type key = int
