(* Copyright (c) 2013 Radek Micek *)

let pick f arr =
  let rec loop i =
    if i < Array.length arr then
      match f arr.(i) with
        | None -> loop (i+1)
        | x -> x
    else
      None in
  loop 0
