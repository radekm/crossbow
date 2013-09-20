(* Copyright (c) 2013 Radek Micek *)

type sign =
  | Pos
  | Neg

let neg = function
  | Pos -> Neg
  | Neg -> Pos

type lbool =
  | Ltrue
  | Lfalse
  | Lundef

module IntSet = BatSet.Make (BatInt)
module IntMap = BatMap.Make (BatInt)
