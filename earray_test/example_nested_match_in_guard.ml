(* Copyright (c) 2013 Radek Micek *)

let a : (int, [`R]) Earray.t = Earray.of_array [| 8 |]
let b : (int, [`R]) Earray.t option = Some (Earray.of_array [| 9 |])

let _ =
  match%earr a with
    | [| i |]
      when
        (match%earr b with
          | Some [| j |] -> i < j
          | _ -> false)
      ->
        1
    | _ -> 2
