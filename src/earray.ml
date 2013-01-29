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

let rindex_of f arr start len =
  let rec loop i =
    if i >= start then
      if f i arr.(i) then
        Some i
      else
        loop (i-1)
    else
      None in
  loop (start + len - 1)

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
        sel 0 (Array.length sel) in
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

let iter_permutations f arr =
  let perm = Array.copy arr in
  let n = Array.length perm in
  Array.sort compare perm;

  (* Generate the lexicographically next permutation. *)
  let next_perm () =
    let idx = rindex_of (fun i x -> x < perm.(i+1)) perm 0 (n - 1) in
    match idx with
      | Some i ->
          let x = perm.(i) in
          let j =  BatOption.get (rindex_of (fun _ y -> y > x) perm 0 n) in
          (* Swap the i-th element with the j-th element. *)
          perm.(i) <- perm.(j);
          perm.(j) <- x;
          (* Reverse the subarray [perm.(i+1), .., perm.(n-1)]. *)
          for k = 0 to ((n-i-1) / 2) - 1 do
            let tmp = perm.(i + 1 + k) in
            perm.(i + 1 + k) <- perm.(n - 1 - k);
            perm.(n - 1 - k) <- tmp
          done;
          true
      | None -> false in

  f perm;
  while next_perm () do
    f perm
  done
