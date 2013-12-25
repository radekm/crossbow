(* Copyright (c) 2013 Radek Micek *)

let arr : (int, [`R]) Earray.t = Earray.of_array [| 2 |]

let _ =
  match%earr arr with
    | [| i |] when i >= 0 -> "nonnegative"
    | [| i |] -> "negative"
    | _ -> "other"
