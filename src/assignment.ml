(* Copyright (c) 2013 Radek Micek *)

type t = int array


(* ************************************************************************* *)
(* Generation *)


let init_assignment a start len = Array.fill a start len 0

let next_assignment a start len adeq_sizes max_size =
  (* Detects whether the i-th element of the assignment can be incremented. *)
  let can_incr i el =
    if adeq_sizes.(i) = 0 || adeq_sizes.(i) >= max_size then
      el + 1 < max_size
    else
      el + 1 < adeq_sizes.(i) in
  (* Find the rightmost element which can be incremented. *)
  match Earray.rindex_of can_incr a start len with
    | None -> false
    | Some i ->
        a.(i) <- a.(i) + 1;
        Array.fill a (i+1) (len - i + start - 1) 0;
        true

let each a start len adeq_sizes max_size f =
  assert (max_size >= 1);

  init_assignment a start len;
  f a;
  while next_assignment a start len adeq_sizes max_size do
    f a
  done

let each_me a start len adeq_sizes max_size f =
  assert (max_size >= 1);

  (* Generate assignments where the i-th element
     is the first occurence of max_el.
  *)
  for i = start to start + len - 1 do
    if adeq_sizes.(i) = 0 || adeq_sizes.(i) >= max_size then begin
      a.(i) <- max_size - 1;
      each a start (i - start) adeq_sizes (max_size - 1)
        (fun _ -> each a (i+1) (start + len - i - 1) adeq_sizes max_size f)
    end
  done

(** [each_me a start len adeq_sizes max_size f] consecutively generates all
   assignments [[|a.(start); ..; a.(start+len-1)|]] wrt [max_size] and
   [adeq_sizes] which are not equivalent up to commutativity
   and which have at least one occurence of [max_el = max_size-1].
   [f a] is called for each generated assignment.

   Note: The length of the assignment must be at least 2.
*)
let each_comm_me a start len adeq_sizes max_size f =
  assert (len >= 2);
  assert (max_size >= 1);
  assert (adeq_sizes.(start) = adeq_sizes.(start+1));

  (* At least one of the first two elements is max_el. *)

  if adeq_sizes.(start) = 0 || adeq_sizes.(start) >= max_size then begin
    a.(start + 1) <- max_size - 1;
    for x = 0 to max_size - 1 do
      a.(start) <- x;
      each a (start + 2) (len - 2) adeq_sizes max_size f
    done
  end;

  (* None of the first two elements is max_el. *)

  let comm_size =
    if adeq_sizes.(start) = 0 || adeq_sizes.(start) >= max_size then
      max_size - 1
    else
      adeq_sizes.(start) in

  for i = start + 2 to start + len - 1 do
    if adeq_sizes.(i) = 0 || adeq_sizes.(i) >= max_size then begin
      a.(i) <- max_size - 1;
      for y = 0 to comm_size - 1 do
        a.(start + 1) <- y;
        for x = 0 to y do
          a.(start) <- x;
          each a (start+2) (i-start-2) adeq_sizes (max_size-1)
            (fun _ -> each a (i+1) (start+len-1-i) adeq_sizes max_size f)
        done
      done
    end
  done


(* ************************************************************************* *)
(* Counting *)


let count start len adeq_sizes max_size =
  let n = ref 1 in
  for i = start to start + len - 1 do
    if adeq_sizes.(i) = 0 || adeq_sizes.(i) >= max_size then
      n := !n * max_size
    else
      n := !n * adeq_sizes.(i)
  done;
  !n

let count_me start len adeq_sizes max_size =
  assert (max_size >= 1);

  count start len adeq_sizes max_size -
  count start len adeq_sizes (max_size-1)

let count_comm_me start len adeq_sizes max_size =
  assert (len >= 2);
  assert (max_size >= 1);
  assert (adeq_sizes.(start) = adeq_sizes.(start+1));

  let nall = count (start+2) (len-2) adeq_sizes max_size in
  let nall_small = count (start+2) (len-2) adeq_sizes (max_size-1) in
  let nall_with_max_el = nall - nall_small in

  if adeq_sizes.(start) = 0 || adeq_sizes.(start) >= max_size then
    (* Number of the assignments of the first two elements
       which are not equivalent up to commutativity
       and where at least one of them is max_el.
    *)
    let nfirst_two_big = max_size in
    (* Number of the assignments of the first two elements
       which are not equivalent up to commutativity
       and where none of them is max_el.
    *)
    let nfirst_two_small = (max_size-1) * max_size / 2 in
    nfirst_two_big * nall + nfirst_two_small * nall_with_max_el
  else
    (* Number of the assignments of the first two elements
       which are not equivalent up to commutativity.
    *)
    let nfirst_two = adeq_sizes.(start) * (adeq_sizes.(start) + 1) / 2 in
    nfirst_two * nall_with_max_el


(* ************************************************************************* *)
(* Ranking *)


(** Let [[|a.(i0); ..; a.(n)|]] be a nonempty ([i0 <= n]) assignment
   wrt [adeq_sizes], [max_el] be the maximal element of the assignment,
   [max_el_idx] be the index of the leftmost occurence of [max_el]
   and [max_size = max_el+1]. A prefix rank is defined
   for [i = i0-1,..,n] and contains:

   - [nbefore] - the number of the assignments [[|b.(i0); ..; b.(i)|]] wrt
     [max_size] and [adeq_sizes] which contain [max_el] before [max_el_idx].
   - [nsame] - the number of the assignments [[|b.(i0); ..; b.(i)|]] wrt
     [max_size] and [adeq_sizes] which have the first occurence of [max_el]
     at [max_el_idx] and which are lexically smaller than
     [[|a.(i0); ..; a.(i)|]].
   - [nsmall] - the number of the assignments
     [[|b.(i0); ..; b.(min i (max_el_idx-1))|]] wrt [max_size] and [adeq_sizes]
     which have no occurence of [max_el].
*)
type prefix_rank = {
  nbefore : int;
  nsame : int;
  nsmall : int;
}

let initial_prefix_rank = { nbefore = 0; nsame = 0; nsmall = 1 }

(** [extend_prefix_rank a adeq_sizes max_size max_el_idx pr i] gets
   the prefix rank [pr] for [i-1] and extends it to the prefix rank for [i].
*)
let extend_prefix_rank a adeq_sizes max_size max_el_idx pr i =
  let big = adeq_sizes.(i) = 0 || adeq_sizes.(i) >= max_size in
  if i < max_el_idx then
    if big then
      {
        nbefore = pr.nbefore * max_size + pr.nsmall;
        nsame = pr.nsame * (max_size - 1) + a.(i);
        nsmall = pr.nsmall * (max_size - 1);
      }
    else
      {
        nbefore = pr.nbefore * adeq_sizes.(i);
        nsame = pr.nsame * adeq_sizes.(i) + a.(i);
        nsmall = pr.nsmall * adeq_sizes.(i);
      }
  else if i > max_el_idx then
    let dsize = if big then max_size else adeq_sizes.(i) in
    {
      pr with
      nbefore = pr.nbefore * dsize;
      nsame = pr.nsame * dsize + a.(i);
    }
  else
    { pr with nbefore = pr.nbefore * max_size }

type rank = int

let rank_me a start len adeq_sizes =
  (* Find the maximal element. *)
  let max_el = ref ~-1 in
  let max_el_idx = ref ~-1 in
  for i = start to start + len - 1 do
    if a.(i) > !max_el then begin
      max_el := a.(i);
      max_el_idx := i
    end
  done;
  let max_size = !max_el + 1 in

  let pr = ref initial_prefix_rank in
  for i = start to start + len - 1 do
    pr := extend_prefix_rank a adeq_sizes max_size !max_el_idx !pr i
  done;
  (!pr).nbefore + (!pr).nsame, !max_el_idx

let rank_comm_me a start len adeq_sizes =
  assert (len >= 2);
  assert (adeq_sizes.(start) = adeq_sizes.(start+1));

  (* Find the maximal element. *)
  let max_el = ref ~-1 in
  let max_el_idx = ref ~-1 in
  for i = start to start + len - 1 do
    if a.(i) > !max_el then begin
      max_el := a.(i);
      max_el_idx := i
    end
  done;
  let max_size = !max_el + 1 in

  (* x <= y *)
  let x, y =
    if a.(start) <= a.(start+1) then
      (a.(start), a.(start+1))
    else
      (a.(start+1), a.(start)) in

  let pr = ref
    begin
      if y = !max_el then
        { nbefore = 0; nsame = x; nsmall = 1 }
      else
        if adeq_sizes.(start) = 0 || adeq_sizes.(start) >= max_size then
          {
            nbefore = max_size;
            nsame = y * (y+1) / 2 + x;
            nsmall = (max_size-1) * max_size / 2;
          }
        else
          {
            nbefore = 0;
            nsame = y * (y+1) / 2 + x;
            nsmall = adeq_sizes.(start) * (adeq_sizes.(start) + 1) / 2;
          }
    end in

  for i = start + 2 to start + len - 1 do
    pr := extend_prefix_rank a adeq_sizes max_size !max_el_idx !pr i
  done;
  (!pr).nbefore + (!pr).nsame, !max_el_idx
