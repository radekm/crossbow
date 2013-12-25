(* Copyright (c) 2013 Radek Micek *)

let arr : (int, [`R]) Earray.t = Earray.of_array [| |]

let _ =
  match%earr arr with
    | [| 1 |] -> 1
    | [| 2 |] | [| 1; 1 |] -> 2
    | [| 3 |] | [| 1; 2 |] | [| 1; 1; 1 |] -> 3
    | _ -> ~-1
