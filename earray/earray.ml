(* Copyright (c) 2013 Radek Micek *)

type ('a,'c) t = 'a array constraint 'c = [< `R|`W]

type 'a rt = ('a, [`R]) t

(* ************************************************************************ *)
(* Functions from module BatArray *)

external length : ('a, [> ]) t -> int = "%array_length"
external get : ('a, [> `R]) t -> 'i -> 'a = "%array_safe_get"
external set : ('a, [> `W]) t -> 'i -> 'a -> unit = "%array_safe_set"

external make : int -> 'a -> ('a, _) t = "caml_make_vect"
let init = BatArray.init

let append = BatArray.append
let concat = BatArray.concat
let sub = BatArray.sub
let copy = BatArray.copy
let fill = BatArray.fill
let blit = BatArray.blit

let to_list = BatArray.to_list
let of_list = BatArray.of_list

let max = BatArray.max
let min = BatArray.min

let sum = BatArray.max
let fsum = BatArray.min

let avg = BatArray.avg
let favg = BatArray.favg

let left = BatArray.left
let right = BatArray.right

let head = BatArray.head
let tail = BatArray.tail

let iter = BatArray.iter
let map = BatArray.map
let iteri = BatArray.iteri
let mapi = BatArray.mapi

let fold_left = BatArray.fold_left
let fold_right = BatArray.fold_right

let modify = BatArray.modify
let modifyi = BatArray.modifyi

let fold_lefti = BatArray.fold_lefti
let fold_righti = BatArray.fold_righti

let reduce = BatArray.reduce

let singleton = BatArray.singleton

let sort = BatArray.sort
let stable_sort = BatArray.stable_sort
let fast_sort = BatArray.fast_sort

let decorate_stable_sort = BatArray.decorate_stable_sort
let decorate_fast_sort = BatArray.decorate_fast_sort

let iter2 = BatArray.iter2
let iter2i = BatArray.iter2i

let for_all2 = BatArray.for_all2
let exists2 = BatArray.exists2

let map2 = BatArray.map2

let for_all = BatArray.for_all
let exists = BatArray.exists

let find = BatArray.find
let mem = BatArray.mem
let memq = BatArray.memq
let findi = BatArray.findi

let filter = BatArray.filter
let filteri = BatArray.filteri
let filter_map = BatArray.filter_map

let find_all = BatArray.find_all
let partition = BatArray.partition


let rev = BatArray.rev
let rev_in_place = BatArray.rev_in_place
let enum = BatArray.enum
let of_enum = BatArray.of_enum
let backwards = BatArray.backwards
let of_backwards = BatArray.of_backwards


let range = BatArray.range
let insert = BatArray.insert

let print = BatArray.print
let compare = BatArray.compare
let ord = BatArray.ord
let equal = BatArray.equal

module Exceptionless = struct
  let find = BatArray.Exceptionless.find
  let findi = BatArray.Exceptionless.findi
end

(* ************************************************************************ *)
(* New functions *)

module Array = struct
  external get : ('a, [> `R]) t -> int -> 'a = "%array_safe_get"
  external set : ('a, [> `W]) t -> int -> 'a -> unit = "%array_safe_set"
end

let empty = [| |]

let is_empty arr = arr = [| |]

external of_array : 'a array -> ('a, _) t = "%identity"
external to_array : ('a, [`R|`W]) t -> 'a array = "%identity"

let of_dyn_array darr = BatDynArray.to_array darr |> of_array

external read_only : ('a, [> `R]) t -> ('a, [`R]) t = "%identity"
external write_only : ('a, [> `W]) t -> ('a, [`W]) t = "%identity"

let pick f arr =
  let rec loop i =
    if i < length arr then
      match f arr.(i) with
        | None -> loop (i+1)
        | x -> x
    else
      None in
  loop 0

let existsi f arr =
  let rec loop i =
    i < length arr &&
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
  let n = length arr in
  if k < 0 || k > n then invalid_arg "k";
  (* Combination. *)
  let comb = sub arr 0 k in
  (* Indices of the items selected in the combination. *)
  let sel = BatArray.init k (fun i -> i) in

  let next_comb () =
    (* Find the rightmost index which can be incremented. *)
    let idx =
      rindex_of
        (fun i x ->
          (* Maximal value of i-th index. *)
          let max_val = n - (k-i) in
          x < max_val)
        sel 0 (length sel) in
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
  let perm = copy arr in
  let n = length perm in
  sort Pervasives.compare perm;

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
