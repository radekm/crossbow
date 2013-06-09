(* Copyright (c) 2013 Radek Micek *)

type sign =
  | Pos
  | Neg

let neg = function
  | Pos -> Neg
  | Neg -> Pos
