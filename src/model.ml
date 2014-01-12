(* Copyright (c) 2013 Radek Micek *)

module Array = Earray.Array

type table = {
  values : (int, [`R]) Earray.t;
}

type t = {
  max_size : int;
  symbs : table Symb.Map.t;
}

module S = Symb
module Ms = Ms_model

let of_ms_model ms_model sorts =
  let max_size = ms_model.Ms.max_size in

  (* One old element (reference element) is chosen for each sort
     which is going to be extended. If possible this element differs
     from all constants in that sort.
  *)
  let reference_elems =
    let adeq_sizes = sorts.Sorts.adeq_sizes in
    let nsorts = Earray.length adeq_sizes in
    let used = Earray.make max_size 0 in
    Earray.init nsorts
      (fun s ->
        if adeq_sizes.(s) > 0 && adeq_sizes.(s) < max_size then
          let consts = sorts.Sorts.consts.(s) in
          let nconsts = Earray.length consts in
          if nconsts < adeq_sizes.(s) then begin
            (* Count how many times is each value of the sort "s"
               used as a result.
            *)
            Earray.iter
              (fun c ->
                let v = (Symb.Map.find c ms_model.Ms.symbs).Ms.values.(0) in
                used.(v) <- used.(v) + 1)
              consts;
            (* Find an unused value. *)
            let v = Earray.findi ((=) 0) used in
            (* Reset array "used" to zeros. *)
            Earray.fill used 0 max_size 0;
            v
          end else
            0
        else
          ~-1) in

  let rank a param_sizes =
    let r = ref 0 in
    for j = 0 to Earray.length param_sizes - 1 do
      r := !r * param_sizes.(j) + a.(j)
    done;
    !r in

  let symbs = ref Symb.Map.empty in
  Symb.Map.iter
    (fun symb table ->
      let symb_sorts = Hashtbl.find sorts.Sorts.symb_sorts symb in
      let old_param_sizes = table.Ms.param_sizes in
      let old_values = table.Ms.values in
      let arity = Earray.length old_param_sizes in
      let new_param_sizes = Earray.make arity max_size in
      let new_values =
        Earray.make (Assignment.count 0 arity new_param_sizes max_size) ~-1 in
      let old_args = Earray.make arity 0 in
      let new_args = Earray.make arity 0 in
      let i = ref 0 in
      (* Fill new_values. *)
      Assignment.each new_args 0 arity new_param_sizes max_size
        (* The value for a new cell is taken from the corresponding old cell.
           To find a corresponding old cell simply replace every new argument
           by the reference element for the sort of the argument.
        *)
        (fun new_args ->
          Earray.iteri
            (fun j v ->
              if v >= old_param_sizes.(j) then
                old_args.(j) <- reference_elems.(symb_sorts.(j))
              else
                old_args.(j) <- v)
            new_args;
          let r = rank old_args old_param_sizes in
          new_values.(!i) <- old_values.(r);
          incr i);
      symbs := Symb.Map.add symb
        { values = Earray.read_only new_values } !symbs)
    ms_model.Ms.symbs;

  { max_size; symbs = !symbs }

let equal a b =
  a.max_size = b.max_size &&
  Symb.Map.equal (=) a.symbs b.symbs

let compare a b =
  let r = compare a.max_size b.max_size in
  if r = 0 then
    Symb.Map.compare compare a.symbs b.symbs
  else
    r

let canonize model =
  let max_size = model.max_size in
  let g = Bliss.create_graph () in
  let used_colors = ref 0 in

  (* Create vertices for domain elements. *)
  for i = 1 to max_size do
    ignore (Bliss.add_vertex g !used_colors)
  done;
  incr used_colors;

  Symb.Map.iter
    (fun symb table ->
      let arity = Symb.arity symb in
      let kind = Symb.kind symb in
      let param_sizes = Earray.make arity max_size in
      let i = ref 0 in
      let len = if kind = S.Pred then arity else arity + 1 in
      let a = Earray.make len 0 in
      (* Colors for the vertices of the symbol. *)
      let colors =
        Earray.init
          len
          (fun _ -> let c = !used_colors in incr used_colors; c) in
      (* For vertices for the symbol (for its parameters and result). *)
      let vs = Earray.make len 0 in
      match arity, kind with
        | 0, S.Pred -> ()
        | _, S.Func ->
            Assignment.each a 0 arity param_sizes max_size
              (fun a ->
                (* Add vertices for parameters and result. *)
                Earray.iteri
                  (fun j _ -> vs.(j) <- Bliss.add_vertex g colors.(j))
                  vs;
                (* Connect parameters to values. *)
                for j = 0 to arity - 1 do
                  Bliss.add_edge g vs.(j) a.(j)
                done;
                (* Connect result to value *)
                Bliss.add_edge
                  g
                  vs.(arity)
                  table.values.(!i);
                incr i;
                (* Connect parameters and result together. *)
                for j = 1 to arity do
                  Bliss.add_edge g vs.(j-1) vs.(j)
                done)
        | _, S.Pred ->
            Assignment.each a 0 arity param_sizes max_size
              (fun a ->
                if table.values.(!i) = 1 then begin
                  (* Add vertices for parameters. *)
                  Earray.iteri
                    (fun j _ -> vs.(j) <- Bliss.add_vertex g colors.(j))
                    vs;
                  (* Connect parameters to values. *)
                  for j = 0 to arity - 1 do
                    Bliss.add_edge g vs.(j) a.(j)
                  done;
                  (* Connect parameters together. *)
                  for j = 1 to arity - 1 do
                    Bliss.add_edge g vs.(j-1) vs.(j)
                  done;
                end;
                incr i))
    model.symbs;

  let lab = Earray.of_array (Bliss.canonical_form g max_size) in
  (* Renaming to canonical form. *)
  let renaming = lab in
  (* Renaming from canonical form to original. *)
  let inv_renaming =
    let inv = Earray.copy renaming in
    for i = 0 to max_size - 1 do
      inv.(renaming.(i)) <- i
    done;
    inv in

  let rank args =
    let r = ref 0 in
    Earray.iter
      (fun arg -> r := !r * max_size + arg)
      args;
    !r in

  let symbs' =
    Symb.Map.mapi
      (fun symb table ->
        let arity = Symb.arity symb in
        let kind = Symb.kind symb in
        match arity, kind with
          | 0, S.Pred -> table
          | _, _ ->
              let param_sizes = Earray.make arity max_size in
              let i = ref 0 in
              let a = Earray.make arity 0 in
              let a2 = Earray.make arity 0 in
              let new_values = Earray.copy table.values in
              Assignment.each a 0 arity param_sizes max_size
                (fun a ->
                  (* Get original result. *)
                  Earray.iteri
                    (fun j v -> a2.(j) <- inv_renaming.(v))
                    a;
                  let orig_result =
                    table.values.(rank a2) in

                  let result =
                    if kind = S.Pred then
                      orig_result
                    else
                      renaming.(orig_result) in
                  new_values.(!i) <- result;
                  incr i);
              { values = Earray.read_only new_values })
      model.symbs in

  { model with symbs = symbs' }

(* Converts the given multi-sorted model to a model with a single sort.
   Sorts are permuted before conversion.

   Array [perms] contains permutation for each sort.

   Note: All domains must have size equal to max_size.
*)
let of_ms_model_perm ms_model sorts perms =
  let max_size = ms_model.Ms.max_size in

  let inv_perms =
    Earray.init
      (Earray.length perms)
      (fun p ->
        let inv = Earray.copy perms.(p) in
        for i = 0 to Earray.length inv - 1 do
          inv.(perms.(p).(i)) <- i
        done;
        inv) in

  let rank args =
    let r = ref 0 in
    Earray.iter
      (fun arg -> r := !r * max_size + arg)
      args;
    !r in

  (* Permute sorts. *)
  let symbs' =
    Symb.Map.mapi
      (fun symb table ->
        let arity = Symb.arity symb in
        let kind = Symb.kind symb in
        match arity, kind with
          | 0, S.Pred -> { values = table.Ms.values }
          | _, _ ->
              let sorts' = Hashtbl.find sorts.Sorts.symb_sorts symb in
              let a = Earray.copy table.Ms.param_sizes in
              let a2 = Earray.copy table.Ms.param_sizes in
              let new_values = Earray.copy table.Ms.values in
              let i = ref 0 in
              Assignment.each a 0 arity table.Ms.param_sizes max_size
                (fun a ->
                  (* Get the original result. *)
                  Earray.iteri
                    (fun j v -> a2.(j) <- inv_perms.(sorts'.(j)).(v))
                    a;
                  let orig_result = table.Ms.values.(rank a2) in

                  let result =
                    if kind = S.Pred
                    then orig_result
                    else perms.(sorts'.(arity)).(orig_result) in
                  new_values.(!i) <- result;
                  incr i);
              { values = Earray.read_only new_values })
      ms_model.Ms.symbs in

  { max_size; symbs = symbs' }

(* Permutes elements of the sorts [1..(nsorts-1)] in the given multi-sorted
   model and for each permutation reconstructs a model with a single sort
   and calls the given function with that model.

   Note: All domain sizes in the multi-sorted model must be equal
   to [max_size].
*)
let iter_all_of_ms_model f ms_model sorts =
  let max_size = ms_model.Ms.max_size in
  let nsorts = Earray.length sorts.Sorts.adeq_sizes in

  (* All domains must have size equal to max_size. *)
  if
    Earray.exists
      (fun size -> size <> 0 && size < max_size)
      sorts.Sorts.adeq_sizes
  then
    failwith "iter_all_of_ms_model: domain sizes";

  let identity = Earray.init max_size (fun i -> i) in
  let perms = Earray.make nsorts identity in
  let rec permute_sorts sort =
    let each_perm p =
      perms.(sort) <- p;
      permute_sorts (sort + 1) in
    if sort < nsorts then
      Earray.iter_permutations each_perm (Earray.init max_size (fun i -> i))
    else
      f (of_ms_model_perm ms_model sorts perms) in

  permute_sorts 1

let all_of_ms_model ms_model sorts =
  let set = ref BatSet.empty in
  iter_all_of_ms_model
    (fun m -> set := BatSet.add (canonize m) !set)
    ms_model sorts;
  !set
