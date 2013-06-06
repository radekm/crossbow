(* Copyright (c) 2013 Radek Micek *)

type table = {
  values : int array;
}

type 's t = {
  max_size : int;
  symbs : ('s Symb.id, table) Hashtbl.t;
}

module Ms = Ms_model

let of_ms_model ms_model sorts =
  let max_size = ms_model.Ms.max_size in

  (* One old element (reference element) is chosen for each sort
     which is going to be extended. If possible this element differs
     from all constants in that sort.
  *)
  let reference_elems =
    let adeq_sizes = sorts.Sorts.adeq_sizes in
    let nsorts = Array.length adeq_sizes in
    let used = Array.make max_size 0 in
    Array.init nsorts
      (fun s ->
        if adeq_sizes.(s) > 0 && adeq_sizes.(s) < max_size then
          let consts = sorts.Sorts.consts.(s) in
          let nconsts = Array.length consts in
          if nconsts < adeq_sizes.(s) then begin
            (* Count how many times is each value of the sort "s"
               used as a result.
            *)
            Array.iter
              (fun c ->
                let v = (Hashtbl.find ms_model.Ms.symbs c).Ms.values.(0) in
                used.(v) <- used.(v) + 1)
              consts;
            (* Find an unused value. *)
            let v = BatArray.findi ((=) 0) used in
            (* Reset array "used" to zeros. *)
            Array.fill used 0 max_size 0;
            v
          end else
            0
        else
          ~-1) in

  let rank a param_sizes =
    let r = ref 0 in
    for j = 0 to Array.length param_sizes - 1 do
      r := !r * param_sizes.(j) + a.(j)
    done;
    !r in

  let model = { max_size; symbs = Hashtbl.create 20 } in
  Hashtbl.iter
    (fun symb table ->
      let symb_sorts = Hashtbl.find sorts.Sorts.symb_sorts symb in
      let old_param_sizes = table.Ms.param_sizes in
      let old_values = table.Ms.values in
      let arity = Array.length old_param_sizes in
      let new_param_sizes = Array.make arity max_size in
      let new_values =
        Array.make (Assignment.count 0 arity new_param_sizes max_size) ~-1 in
      let old_args = Array.make arity 0 in
      let new_args = Array.make arity 0 in
      let i = ref 0 in
      (* Fill new_values. *)
      Assignment.each new_args 0 arity new_param_sizes max_size
        (* The value for a new cell is taken from the corresponding old cell.
           To find a corresponding old cell simply replace every new argument
           by the reference element for the sort of the argument.
        *)
        (fun new_args ->
          Array.iteri
            (fun j v ->
              if v >= old_param_sizes.(j) then
                old_args.(j) <- reference_elems.(symb_sorts.(j))
              else
                old_args.(j) <- v)
            new_args;
          let r = rank old_args old_param_sizes in
          new_values.(!i) <- old_values.(r);
          incr i);
      Hashtbl.add model.symbs symb { values = new_values })
    ms_model.Ms.symbs;

  model
