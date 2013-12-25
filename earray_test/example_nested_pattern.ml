(* Copyright (c) 2013 Radek Micek *)

let arr : ((int, [`R]) Earray.t, [`R]) Earray.t option = None

let _ =
  match%earr arr with
    | None -> 1
    | Some [| [| _ |]; [| x; 17 |]; ys |] -> 2
    | Some _ -> 3
