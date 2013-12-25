(* Copyright (c) 2013 Radek Micek *)

let a : (int, [`R]) Earray.t = Earray.of_array [| 1; 2; 3 |]

let _ =
  match%earr a with
    | [| |] -> "empty"
    | [| _; x; 3 |] -> "matches"
    | _ -> "fallback"
