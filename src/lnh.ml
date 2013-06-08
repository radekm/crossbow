(* Copyright (c) 2013 Radek Micek *)

module IntSet = BatSet.IntSet

let (|>) = BatPervasives.(|>)

type 's t = {
  assig : 's Symred.cell * int;
  required : 's Symred.cell array * int;
}

let lnh symbols sorts proc_cells ((symb, args), (lo, hi)) =
  let get_sorts symb = Hashtbl.find sorts.Sorts.symb_sorts symb in
  let get_res_sort symb = (get_sorts symb).(Symb.arity symb) in
  let sort = get_res_sort symb in

  (* Potentially unused values from the interval (lo, hi). *)
  let po_unused =
    (* Values used as arguments are certaintly used. *)
    let used_as_argument =
      let proc_args symb args =
        let sorts = get_sorts symb in
        BatArray.fold_lefti
          (fun xs i x ->
            if sorts.(i) = sort
            then IntSet.add x xs
            else xs)
          IntSet.empty
          args in
      List.fold_left
        (fun xs ((symb, args), _) ->
          IntSet.union xs (proc_args symb args))
        (proc_args symb args)
        proc_cells in
    (* Values from singleton ranges. If the range contains
       only one value then this values is certainly used.
    *)
    let used_as_result =
      List.fold_left
        (fun xs ((symb, _), (lo, hi)) ->
          if lo = hi && get_res_sort symb = sort
          then IntSet.add lo xs
          else xs)
        IntSet.empty
        proc_cells in
    let used = IntSet.union used_as_argument used_as_result in
    BatEnum.(lo -- hi)
    |> BatEnum.filter (fun v -> IntSet.mem v used |> not)
    |> BatArray.of_enum in

  if po_unused = [| |] then
    [| |]
  else
    (* Create constraint for each potentially unused value
       except the first.
    *)
    Array.init
      (Array.length po_unused - 1)
      (fun i ->
        let prev = po_unused.(i) in
        let u = po_unused.(i+1) in

        (* Cells which can be assigned [prev]. *)
        let cells : 's Symred.cell array =
          let cond ((symb, _), (lo, hi)) =
            get_res_sort symb = sort && lo <= prev && prev <= hi in
          proc_cells
          |> BatList.filter cond
          |> BatList.map fst
          |> Array.of_list in

        (* Potentially unused value [u] can be assigned to the cell
           [(symb, args)] only when the previous potentially unused value
           [prev] is assigned to some cell from [cells].
        *)
        {
          assig = (symb, args), u;
          required = cells, prev;
        })
