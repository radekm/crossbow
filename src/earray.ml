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

let existsi f arr =
  let rec loop i =
    i < Array.length arr &&
    (f i arr.(i) || loop (i+1)) in
  loop 0

let rindex_of f arr =
  let rec loop i =
    if i >= 0 then
      if f i arr.(i) then
        Some i
      else
        loop (i-1)
    else
      None in
  loop (Array.length arr - 1)

let iter_combinations f k arr =
  let n = Array.length arr in
  if k < 0 || k > n then invalid_arg "k";
  (* Combination. *)
  let comb = Array.sub arr 0 k in
  (* Indices of the items selected in the combination. *)
  let sel = Array.init k (fun i -> i) in

  let next_comb () =
    (* Find the rightmost index which can be incremented. *)
    let idx =
      rindex_of
        (fun i x ->
          (* Maximal value of i-th index. *)
          let max_val = n - (k-i) in
          x < max_val)
        sel in
    match idx with
      | Some i ->
          sel.(i) <- sel.(i) + 1;
          comb.(i) <- arr.(sel.(i));
          for j = i + 1 to k - 1 do
            sel.(j) <- sel.(j - 1) + 1;
            comb.(j) <- arr.(sel.(j))
          done;
          true
      | None ->
          false in

  f comb;
  while next_comb () do
    f comb
  done
