(* Copyright (c) 2013 Radek Micek *)

type t = {
  params : int array;
  values : int array;
}

let create arity adeq_sizes max_size =
  let params =
    Array.init arity (fun i ->
      if adeq_sizes.(i) = 0 || adeq_sizes.(i) >= max_size
      then max_size
      else adeq_sizes.(i)) in
  { params; values = Array.make (Array.fold_left ( * ) 1 params) 0 }

let compute_idx params args =
  let idx = ref 0 in
  for i = 0 to Array.length params - 1 do
    idx := !idx * params.(i) + args.(i)
  done;
  !idx

let get i args = i.values.(compute_idx i.params args)

let set i args v = i.values.(compute_idx i.params args) <- v
