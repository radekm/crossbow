(* Copyright (c) 2013 Radek Micek *)

type args = int array

type 's cell = 's Symb.id * args

type commutative = bool

type 's t = {
  (* Constant fields. *)

  symb_sorts : ('s Symb.id, Sorts.sort_id array) Hashtbl.t;
  (* Sorts of predicate and function symbols. *)

  adeq_sizes : int array;
  (* [adeq_sizes.(i)] is the adequate domain size of the sort [i]. *)

  consts : 's Symb.id array array;
  (* [consts.(i)] is an array of the constants of the sort [i]. *)

  funcs : ('s Symb.id * commutative) array array;
  (* [funcs.(i)] is an array of the function symbols of the sort [i]
     with arity > 0.
  *)

  distinct_consts : 's Symb.id array;
  (* Constants which must be assigned distinct values.
     All belong to the same sort.
  *)

  (* Variable fields. *)

  mutable max_size : int;
  (* Maximum domain size. *)

  used_elems : int array;
  (* Number of the used elements in each sort. *)

  mutable assigned : 's cell BatSet.t;
  (* Cells already assigned by symmetry reduction. *)
}

let (|-) = BatPervasives.(|-)

let create prob sorts =
  let nsorts = Array.length sorts.Sorts.adeq_sizes in
  let funcs_by_sorts () =
    let arr = Array.make nsorts [] in
    let each_symb s sorts =
      if
        (* s is a function symbol *)
        Array.length sorts = Symb.arity s + 1 &&
        (* s has arity > 0 *)
        Array.length sorts > 1
      then
        let res_sort = sorts.(Array.length sorts - 1) in
        let symb = s, Symb.commutative prob.Prob.symbols s in
        arr.(res_sort) <- symb :: arr.(res_sort) in
    Hashtbl.iter each_symb sorts.Sorts.symb_sorts;
    Array.map (BatList.sort |- Array.of_list) arr in
  {
    symb_sorts = sorts.Sorts.symb_sorts;
    adeq_sizes = sorts.Sorts.adeq_sizes;
    consts = sorts.Sorts.consts;
    funcs = funcs_by_sorts ();
    distinct_consts = BatDynArray.to_array prob.Prob.distinct_consts;
    max_size = 0;
    used_elems = Array.make nsorts 0;
    assigned = BatSet.empty;
  }

let assign_distinct_constant sr result =
  if sr.max_size - 1 < Array.length sr.distinct_consts then begin
    (* Assign distinct constant c to value v. *)
    let c = sr.distinct_consts.(sr.max_size - 1) in
    let sort = (Hashtbl.find sr.symb_sorts c).(0) in
    let cell = c, [| |] in
    let v = sr.used_elems.(sort) in
    let range = v, v in
    result := [ cell, range ];
    sr.assigned <- BatSet.add cell sr.assigned;

    (* Mark value v as used. *)
    sr.used_elems.(sort) <- v + 1
  end

let assign_constants sr total_elems result =
  let find_unassigned_const sort =
    BatArray.Exceptionless.find
      (fun c -> not (BatSet.mem (c, [| |]) sr.assigned))
      sr.consts.(sort) in

  let rec loop () =
    let again = ref false in

    for sort = 0 to Array.length sr.adeq_sizes - 1 do
      if sr.used_elems.(sort) < total_elems.(sort) then
        match find_unassigned_const sort with
          | None -> ()
          | Some c ->
              (* Assign constant c to the interval 0..v. *)
              let cell = c, [| |] in
              let v = sr.used_elems.(sort) in
              let range = 0, v in
              result := (cell, range) :: !result;
              sr.assigned <- BatSet.add cell sr.assigned;

              (* Mark value v as used. *)
              sr.used_elems.(sort) <- v + 1;

              again := true
    done;

    if !again then
      loop () in

  loop ()

let assign_funcs (type s) (sr : s t) total_elems result =
  let module M = struct
    type t = s
    exception Unassigned_cell of t cell
  end in

  (* Finds an unassigned cell where the arguments are some already used
     elements.
  *)
  let find_unassigned_cell sort =
    try
      Array.iter
        (fun (f, commutative) ->
          let sorts = Hashtbl.find sr.symb_sorts f in
          let arity = Array.length sorts - 1 in
          (* Adequate domain size is the number of the used elements. *)
          let adeq_sizes =
            Array.init arity (fun i -> sr.used_elems.(sorts.(i))) in
          (* Check that no adequate size is 0 (i.e. there is at least
             one used element in the domain of every parameter).
          *)
          if BatArray.for_all (fun size -> size > 0) adeq_sizes then begin
            let gen =
              if commutative then
                Assignment.each_comm_me
              else
                Assignment.each_me in
            let a = Array.make arity 0 in
            (* Try all assignments which contain only used elements. *)
            for max_size = 1 to sr.max_size do
              gen a 0 arity adeq_sizes max_size
                (fun a ->
                  let cell = f, a in
                  if not (BatSet.mem cell sr.assigned) then
                    raise (M.Unassigned_cell cell))
            done
          end)
        sr.funcs.(sort);
      None
    with
      | M.Unassigned_cell cell -> Some cell in

  (* Marks some unused elements as used so that find_unassigned_cell
     succeeds. Returns false if no elements were marked.
  *)
  let use_new_elem_as_arg () =
    let no_used_elem _ sort = sr.used_elems.(sort) = 0 in
    try
      (* Marks an unused element in esort as used and checks
         if it created a cell for find_unassigned_cell.
      *)
      for esort = 0 to Array.length sr.adeq_sizes - 1 do
        if sr.used_elems.(esort) < total_elems.(esort) then begin
          sr.used_elems.(esort) <- sr.used_elems.(esort) + 1;

          (* Unassigned cell is: the arguments from esort are set to
             the biggest used element and the other arguments are set to zero.
          *)
          let has_unassigned_cell f =
            let sorts = Hashtbl.find sr.symb_sorts f in
            let arity = Array.length sorts - 1 in

            let is_esort _ sort = sort = esort in

            (* Every argument can be set to some used element. *)
            Earray.rindex_of no_used_elem sorts 0 arity = None &&
            (* Setting argument from esort to the biggest used element
               guarantees that the cell is unassigned - so a parameter
               from esort must be present.
            *)
            Earray.rindex_of is_esort sorts 0 arity <> None in

          (* Checks if there is a function symbol with an unassigned cell which
             can be assigned an unused element.
          *)
          for sort = 0 to Array.length sr.adeq_sizes - 1 do
            if sr.used_elems.(sort) < total_elems.(sort) then
              Array.iter
                (fun (f, _) -> if has_unassigned_cell f then raise Exit)
                sr.funcs.(sort)
          done;

          sr.used_elems.(esort) <- sr.used_elems.(esort) - 1
        end
      done;

      (* For function symbols which have a parameter from a sort without a used
         element. If a symbol has two or more such parameters from distinct
         sorts then it won't be handled by the code above.
      *)
      for res_sort = 0 to Array.length sr.adeq_sizes - 1 do
        if sr.used_elems.(res_sort) < total_elems.(res_sort) then begin
          Array.iter
            (fun (f, _) ->
              let sorts = Hashtbl.find sr.symb_sorts f in
              let arity = Array.length sorts - 1 in
              (* There is a parameter sort without used elements. *)
              if Earray.rindex_of no_used_elem sorts 0 arity <> None then begin
                (* Sorts where unused element were marked as used. *)
                let esorts = Queue.create () in

                for p = 0 to arity - 1 do
                  let sort = sorts.(p) in
                  if no_used_elem ~-1 sort then
                    let _ = Queue.push sort esorts in
                    sr.used_elems.(sort) <- sr.used_elems.(sort) + 1
                done;

                if sr.used_elems.(res_sort) < total_elems.(res_sort) then
                  raise Exit;

                while not (Queue.is_empty esorts) do
                  let sort = Queue.pop esorts in
                  sr.used_elems.(sort) <- sr.used_elems.(sort) - 1
                done
              end)
            sr.funcs.(res_sort)
        end
      done;

      false
    with
      | Exit -> true in

  let rec loop () =
    let again = ref false in

    for sort = 0 to Array.length sr.adeq_sizes - 1 do
      if sr.used_elems.(sort) < total_elems.(sort) then
        match find_unassigned_cell sort with
          | None -> ()
          | Some cell ->
              (* Assign cell to the interval 0..v. *)
              let v = sr.used_elems.(sort) in
              let range = 0, v in
              result := (cell, range) :: !result;
              sr.assigned <- BatSet.add cell sr.assigned;

              (* Mark value v as used. *)
              sr.used_elems.(sort) <- v + 1;

              again := true
    done;

    if !again || use_new_elem_as_arg () then
      loop () in

  loop ()

let incr_max_size sr =
  sr.max_size <- sr.max_size + 1;
  let total_elems =
    Array.map
      (fun i -> if i = 0 || i >= sr.max_size then sr.max_size else i)
      sr.adeq_sizes in

  (* Cells and their values. *)
  let result = ref [] in

  assign_distinct_constant sr result;
  assign_constants sr total_elems result;
  assign_funcs sr total_elems result;

  List.rev !result
