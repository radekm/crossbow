(* Copyright (c) 2013 Radek Micek *)

module T = Term
module C = Clause

module IntSet = BatSet.IntSet

let (|>) = BatPervasives.(|>)
let (|-) = BatPervasives.(|-)

(* For each variable count to how many variables it is connected.

   n is the number of variables. Input clause must be normalized.
*)
let count_connections n norm_lits : int array =
  (* 0 for not connected, 1 for connected. *)
  let connected = Array.make_matrix n n 0 in
  List.iter
    (fun lit ->
      let vars = T.vars lit in
      IntSet.iter
        (fun x -> IntSet.iter (fun y -> connected.(x).(y) <- 1) vars)
        vars)
    norm_lits;
  Array.init n (fun x -> Array.fold_left (+) 0 connected.(x))

let paradox_binary_split
    (partition_lits : T.var -> 's C.lit list -> 's C.lit list * 's C.lit list)
    new_pred
    lits =
  let cl, n = C.normalize_vars { C.cl_id = -1; C.cl_lits = lits } in
  let lits = cl.C.cl_lits in
  let nconns = count_connections n lits in
  (* Don't split clauses with 3 or less variables. *)
  if n <= 3 then
    ([lits], [])
  else
    (* Variable which is connected to the least number of other variables. *)
    let x =
      BatArray.fold_lefti
        (fun min_i i _ -> if nconns.(i) < nconns.(min_i) then i else min_i)
        0
        nconns in
    (* No proper splitting can be performed when all variables
       are connected to each other.
    *)
    if nconns.(x) = n then
      ([lits], [])
    else
      let left, right = partition_lits x lits in
      let shared_vars =
        IntSet.inter
          (T.vars_of_many left)
          (T.vars_of_many right) in
      let p = new_pred (IntSet.cardinal shared_vars) in
      let atom =
        let args =
          IntSet.enum shared_vars
          |> BatEnum.map (fun x -> T.Var x)
          |> BatArray.of_enum in
        T.Func (p, args) in
      ([], [atom :: left; C.neg_lit atom :: right])

type 's t = (int -> 's Symb.id) -> 's C.lit list -> 's C.lit list list

(* Repeatedly apply splitting to the given clause. *)
let binary_splitting split new_pred lits =
  let rec loop finished = function
    | [] -> finished
    | lits :: rest ->
        let fin, unfin = split new_pred lits in
        loop (List.rev_append fin finished) (List.rev_append unfin rest) in
  loop [] [lits]

let paradox_splitting new_pred lits =
  let partition_lits x lits =
    BatList.partition (T.vars |- IntSet.mem x) lits in
  binary_splitting (paradox_binary_split partition_lits) new_pred lits

let paradox_mod_splitting new_pred lits =
  let partition_lits x lits =
    let l, r = BatList.partition (T.vars |- IntSet.mem x) lits in
    let lvars = Term.vars_of_many l in
    (* Literals in l' contain only variables from lvars.
       Moving these literals from the right to the left can
       decrease the number of variable on the right and in the intersection.
    *)
    let l', r' =
      BatList.partition (fun lit -> IntSet.subset (T.vars lit) lvars) r in
    l @ l', r' in
  let ground, nonground =
    BatList.partition (T.vars |- IntSet.is_empty) lits in
  let clauses =
    binary_splitting
      (paradox_binary_split partition_lits)
      new_pred
      nonground in
  (* Add ground literals to a clause which has the (lexicographically)
     smallest (number of variables, number of literals).
  *)
  match clauses with
    | [] -> failwith "paradox_mod_splitting: no clauses"
    | c :: cs ->
        let count_vars_lits c =
          IntSet.cardinal (Term.vars_of_many c), List.length c in
        let best_stats = ref (count_vars_lits c) in
        let best_idx = ref 0 in
        BatList.iteri
          (fun i c ->
            let stats = count_vars_lits c in
            if stats < !best_stats then begin
              best_stats := stats;
              best_idx := i;
            end)
          cs;
        (* Add ground literals. *)
        BatList.mapi
          (fun i c ->
            if i = !best_idx
            then c @ ground
            else c)
          clauses

let split_clause splitting p cl =
  let new_pred arity =
    let s = Symb.add_pred p.Prob.symbols arity in
    Symb.set_auxiliary p.Prob.symbols s true;
    s in
  let clauses = splitting new_pred cl.C.cl_lits in
  BatList.map
    (fun cl_lits -> { C.cl_id = Prob.fresh_id p; C.cl_lits })
    clauses
