(* Copyright (c) 2013 Radek Micek *)

let rec contains x = function
  | [] -> false
  | y :: ys -> y = x || contains x ys

let existsi f =
  let rec loop i = function
    | [] -> false
    | x :: xs -> f i x || loop (i+1) xs in
  loop 0

let rec pick f = function
  | [] -> None
  | x :: xs ->
      match f x with
        | None -> pick f xs
        | x -> x

let picki f =
  let rec loop i = function
    | [] -> None
    | x :: xs ->
        match f i x with
          | None -> loop (i+1) xs
          | x -> x in
  loop 0

let pick_and_remove f =
  let rec loop acc = function
    | [] -> None, List.rev acc
    | x :: xs ->
        match f x with
          | None -> loop (x::acc) xs
          | x -> x, List.rev_append acc xs in
  loop []
