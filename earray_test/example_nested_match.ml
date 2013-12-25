(* Copyright (c) 2013 Radek Micek *)

let arr = [| Some 4 |]
let earr : (int option, [`R]) Earray.t = Earray.of_array [| Some 5 |]

let _ =
  match%earr earr with
    | [| Some x |] ->
        (match arr with
          | [| Some y |] ->
              (match%earr earr with
                | [| Some 5 |] -> 1
                | _ -> 2)
          | _ -> 3)
    | _ -> 4
