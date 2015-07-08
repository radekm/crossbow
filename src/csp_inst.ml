(* Copyright (c) 2013, 2015 Radek Micek *)

module type Inst_sig = sig
  type solver
  type t

  val create : ?nthreads:int -> [> `R] Prob.t -> Sorts.t -> int -> t

  val destroy : t -> unit

  val solve : t -> Sh.lbool

  val solve_timed : t -> int -> Sh.lbool * bool

  val construct_model : t -> Ms_model.t

  val get_solver : t -> solver
end

module Make (Solv : Csp_solver.S) :
  Inst_sig with type solver = Solv.t =
struct
  type solver = Solv.t

  module Array = Earray.Array
  module S = Symb
  module T = Term
  module L = Lit

  type t = {
    solver : Solv.t;
    symbols : [`R] Symb.db;
    sorts : Sorts.t;

    (* Domain size. *)
    n : int;

    (* For each symbol contains domain sizes of its argument sorts
       and result sort (if any).

       [dom_sizes] can be used in places where [adeq_sizes] is needed.
       The difference from [adeq_sizes] is that [dom_sizes] contains
       no zeros and no sizes bigger than [n].
    *)
    dom_sizes : (Symb.id, (int, [`R]) Earray.t) BatMap.t;

    (* Variables for predicates. *)
    pred_arrays : (S.id, (bool Solv.var, [`R]) Earray.t) Hashtbl.t;

    (* Variables for functions. *)
    func_arrays : (S.id, (int Solv.var, [`R]) Earray.t) Hashtbl.t;

    (* Arrays of CSP variables for bool_element constraint. *)
    preds : (S.id, bool Solv.var_array) Hashtbl.t;

    (* Arrays of CSP variables for int_element constraint. *)
    funcs : (S.id, int Solv.var_array) Hashtbl.t;

    (* Maps the result of x1 * c1 + x2 * c2 + x3 * c3 + ... + c to variable y.
       Variables xi in polynomial are sorted and unique.
    *)
    linear : ((int Solv.var * int, [`R]) Earray.t * int, int Solv.var)
      Hashtbl.t;

    bool_element : (S.id * int Solv.var, bool Solv.var) Hashtbl.t;

    int_element : (S.id * int Solv.var, int Solv.var) Hashtbl.t;

    (* Variables in equality are sorted. *)
    eq_var_var : (int Solv.var * int Solv.var, bool Solv.var) Hashtbl.t;

    eq_var_const : (int Solv.var * int, bool Solv.var) Hashtbl.t;

    mutable can_construct_model : bool;
  }

  let dummy_bool_var : bool Solv.var = Obj.magic 0

  let get_var = function
    | T.Var x -> x
    | T.Func _ -> failwith "get_var"

  type index =
    | I_const of int
    | I_var of int Solv.var

  (* Computes index into the array representing a symbol table.
     [a] is assignment of variables.

     [dom_sizes] contains domain sizes of [args].
  *)
  let rec index
      (inst : t)
      (a : (int, [> `R]) Earray.t)
      (dom_sizes : (int, [> `R]) Earray.t)
      (args : (T.t, [> `R]) Earray.t) : index =

    (* Index can be computed statically. *)
    if Earray.for_all T.is_var args then
      I_const (Earray.fold_lefti
                 (fun acc i x -> acc * dom_sizes.(i) + a.(get_var x))
                 0 args)
    else begin
      (* Maps CSP variables for cells to their coefficients. *)
      let vars = ref BatMap.empty in
      (* Maps CSP variables for cells to their domain sizes.
         These size are used when computing [max_idx].
      *)
      let var_dom_sizes = ref BatMap.empty in
      let c, _ =
        Earray.fold_righti
          (fun i arg (c, mult) ->
            match arg with
              | T.Var x -> (c + a.(x) * mult, mult * dom_sizes.(i))
              | T.Func (s, _) ->
                  (* Get CSP variable for cell [arg]. *)
                  let y = var_for_func_term inst a arg in
                  vars :=
                    BatMap.modify_def 0 y (fun coef -> coef + mult) !vars;
                  (* Domain size of the CSP variable [y] is the size of the
                     result sort of [arg] which is the size of [i]-th argument
                     sort which is [dom_sizes.(i)].
                  *)
                  var_dom_sizes :=
                    BatMap.add y dom_sizes.(i) !var_dom_sizes;
                  (c, mult * dom_sizes.(i)))
          args (0, 1) in
      let vars = BatMap.enum !vars |> Earray.of_enum in
      let var_dom_sizes =
        (* [BatMap.values] is not used since its documentation
           doesn't guarantee that the values are in increasing order
           by their keys.
        *)
        BatMap.enum !var_dom_sizes
        |> BatEnum.map snd
        |> Earray.of_enum in
      let nvars = Earray.length vars in
      match%earr vars with
        (* Single variable x with coefficient 1 and no constant term -
           we don't need linear constraint.
        *)
        | [| x, 1 |] when c = 0 -> I_var x
        | _ ->
            let key = (vars, c) in
            (* Variable satisfying linear constraint. *)
            try
              I_var (Hashtbl.find inst.linear key)
            with
              | Not_found ->
                  (* Maximal value of CSP variable [y]. *)
                  let max_idx =
                    Earray.fold_lefti
                      (fun max_idx i (_, coef) ->
                        let max_el = var_dom_sizes.(i) - 1 in
                        max_idx + max_el * coef)
                      c
                      vars in
                  let dom_size = max_idx + 1 in
                  let y = Solv.new_tmp_int_var inst.solver dom_size in
                  (* Post c(0) * x(0) + c(1) * x(1) + ... + c = y as
                          c(0) * x(0) + c(1) * x(1) + ... - y = -c.
                     x(i) is (fst vars.(i)) and
                     c(i) is (snd vars.(i)).
                  *)
                  Solv.linear
                    inst.solver
                    (Earray.init (nvars + 1)
                       (fun i -> if i < nvars then fst vars.(i) else y))
                    (Earray.init (nvars + 1)
                       (fun i -> if i < nvars then snd vars.(i) else ~-1))
                    ~-c;
                  Hashtbl.add inst.linear key y;
                  I_var y
    end

  (* [a] is assignment of variables. *)
  and var_for_func_term
      (inst : t)
      (a : (int, [> `R]) Earray.t) : T.t -> int Solv.var = function

    | T.Var _ -> failwith "var_for_func_term"
    | T.Func (s, args) ->
        let dom_sizes = BatMap.find s inst.dom_sizes in
        match index inst a dom_sizes args with
          | I_const i ->
              (Hashtbl.find inst.func_arrays s).(i)
          | I_var i ->
              let key = (s, i) in
              (* Variable satisfying int_element constraint. *)
              try
                Hashtbl.find inst.int_element key
              with
                | Not_found ->
                    let dom_size = dom_sizes.(Symb.arity s) in
                    let y = Solv.new_tmp_int_var inst.solver dom_size in
                    Solv.int_element
                      inst.solver
                      (Hashtbl.find inst.funcs s) i y;
                    Hashtbl.add inst.int_element key y;
                    y

  (* [a] is assignment of variables. *)
  let var_for_noneq_atom
      (inst : t)
      (a : (int, [> `R]) Earray.t)
      (s : S.id)
      (args : (T.t, [> `R]) Earray.t) : bool Solv.var =

    assert (s <> S.sym_eq);
    let dom_sizes = BatMap.find s inst.dom_sizes in
    match index inst a dom_sizes args with
      | I_const i ->
          (Hashtbl.find inst.pred_arrays s).(i)
      | I_var i ->
          let key = (s, i) in
          (* Variable satisfying bool_element constraint. *)
          try
            Hashtbl.find inst.bool_element key
          with
            | Not_found ->
                let y = Solv.new_tmp_bool_var inst.solver in
                Solv.bool_element
                  inst.solver
                  (Hashtbl.find inst.preds s) i y;
                Hashtbl.add inst.bool_element key y;
                y

  (* [a] is assignment of variables. *)
  let var_for_eq_atom
      (inst : t)
      (a : (int, [> `R]) Earray.t)
      (l : T.t)
      (r : T.t) : bool Solv.var =

    match l, r with
      | T.Var _, T.Var _ -> failwith "var_for_eq_atom"
      | (T.Func _ as f), T.Var x
      | T.Var x, (T.Func _ as f) ->
          let v = var_for_func_term inst a f in
          let key = (v, a.(x)) in
          (try
             Hashtbl.find inst.eq_var_const key
           with
             | Not_found ->
                 let y = Solv.new_tmp_bool_var inst.solver in
                 Solv.eq_var_const
                   inst.solver
                   v a.(x)
                   y;
                 Hashtbl.add inst.eq_var_const key y;
                 y)
      | (T.Func _ as f), (T.Func _ as g) ->
          let v = var_for_func_term inst a f in
          let v' = var_for_func_term inst a g in
          let key = if v <= v' then (v, v') else (v', v) in
          (* Variable satisfying eq_var_var constraint. *)
          (try
             Hashtbl.find inst.eq_var_var key
           with
             | Not_found ->
                 let y = Solv.new_tmp_bool_var inst.solver in
                 Solv.eq_var_var
                   inst.solver
                   (fst key) (snd key)
                   y;
                 Hashtbl.add inst.eq_var_var key y;
                 y)

  (* [var_adeq_sizes] are adequate domain sizes for variables
     from the clause.
  *)
  let instantiate_clause
      (inst : t)
      (var_adeq_sizes : (int, [> `R]) Earray.t)
      (var_eqs : (T.var * T.var, [> `R]) Earray.t)
      (var_ineqs : (T.var * T.var, [> `R]) Earray.t)
      (pos_eq_lits : (T.t * T.t, [> `R]) Earray.t)
      (neg_eq_lits : (T.t * T.t, [> `R]) Earray.t)
      (pos_noneq_lits : (S.id * (T.t, [> `R]) Earray.t, [> `R]) Earray.t)
      (neg_noneq_lits : (S.id * (T.t, [> `R]) Earray.t, [> `R]) Earray.t)
      : unit =

    let nvars = Earray.length var_adeq_sizes in

    (* Arrays for literals. *)
    let pos =
      Earray.make
        (Earray.length pos_eq_lits + Earray.length pos_noneq_lits)
        dummy_bool_var in
    let neg =
      Earray.make
        (Earray.length neg_eq_lits + Earray.length neg_noneq_lits)
        dummy_bool_var in

    Assignment.each (Earray.make nvars 0) 0 nvars var_adeq_sizes inst.n
      (fun a ->
        (* If no (in)equality of variables is satisfied. *)
        if
          Earray.for_all (fun (x, x') -> a.(x) <> a.(x')) var_eqs &&
          Earray.for_all (fun (x, x') -> a.(x) = a.(x')) var_ineqs
        then begin
          (* Positive literals. *)
          Earray.iteri
            (fun i (l, r) -> pos.(i) <- var_for_eq_atom inst a l r)
            pos_eq_lits;
          let skip = Earray.length pos_eq_lits in
          Earray.iteri
            (fun i (s, args) ->
              pos.(skip + i) <- var_for_noneq_atom inst a s args)
            pos_noneq_lits;

          (* Negative literals. *)
          Earray.iteri
            (fun i (l, r) -> neg.(i) <- var_for_eq_atom inst a l r)
            neg_eq_lits;
          let skip = Earray.length neg_eq_lits in
          Earray.iteri
            (fun i (s, args) ->
              neg.(skip + i) <- var_for_noneq_atom inst a s args)
            neg_noneq_lits;

          Solv.clause inst.solver pos neg
        end)

  (* Create CSP variables for symbol. *)
  let add_symb
      (solver : Solv.t)
      (n : int)
      (arity : int)
      (dom_sizes : (int, [> `R]) Earray.t)
      (commutative : bool)
      (new_t_var : Solv.t -> 'a Solv.var)
      (new_t_var_array : Solv.t -> ('a Solv.var, [> `R]) Earray.t ->
       'a Solv.var_array)
      : 'a Solv.var_array * ('a Solv.var, [`R]) Earray.t  =

    if commutative then begin
      let count = ref 0 in
      let starts = Earray.make n 0 in
      for max_size = 1 to n do
        starts.(max_size-1) <- !count;
        count := !count + Assignment.count_comm_me 0 arity dom_sizes max_size
      done;
      let distinct_vars = Earray.init !count (fun _ -> new_t_var solver) in
      let vars = BatDynArray.create () in
      Assignment.each (Earray.make arity 0) 0 arity dom_sizes n
        (fun a ->
          let r, max_el_idx = Assignment.rank_comm_me a 0 arity dom_sizes in
          let var_idx = r + starts.(a.(max_el_idx)) in
          let var = distinct_vars.(var_idx) in
          BatDynArray.add vars var);
      let vars = Earray.of_dyn_array vars in
      (new_t_var_array solver vars, vars)
    end else begin
      let count = Assignment.count 0 arity dom_sizes n in
      let vars = Earray.init count (fun _ -> new_t_var solver) in
      (new_t_var_array solver vars, vars)
    end

  let add_pred inst s =
    let dom_sizes = BatMap.find s inst.dom_sizes in
    if not (Hashtbl.mem inst.preds s) then begin
      let new_var =
        if Symb.auxiliary inst.symbols s
        then Solv.new_tmp_bool_var
        else Solv.new_bool_var in
      let vars, arr =
        add_symb
          inst.solver
          inst.n
          (Symb.arity s)
          dom_sizes
          (Symb.commutative inst.symbols s)
          new_var
          Solv.new_bool_var_array in
      Hashtbl.add inst.pred_arrays s arr;
      Hashtbl.add inst.preds s vars
    end

  let add_func inst s =
    let arity = Symb.arity s in
    let dom_sizes = BatMap.find s inst.dom_sizes in
    if not (Hashtbl.mem inst.funcs s) then begin
      let new_var =
        if Symb.auxiliary inst.symbols s
        then Solv.new_tmp_int_var
        else Solv.new_int_var in
      let vars, arr =
        add_symb
          inst.solver
          inst.n
          arity
          dom_sizes
          (Symb.commutative inst.symbols s)
          (fun solver -> new_var solver dom_sizes.(arity))
          Solv.new_int_var_array in
      Hashtbl.add inst.func_arrays s arr;
      Hashtbl.add inst.funcs s vars
    end

  let each_clause inst clause_id lits =
    (* Adequate domain sizes for variables from the clause. *)
    let var_adeq_sizes =
      let nvars = Sh.IntSet.cardinal (Clause.vars lits) in
      Earray.init
        nvars
        (fun x ->
          let sort = Hashtbl.find inst.sorts.Sorts.var_sorts (clause_id, x) in
          inst.sorts.Sorts.adeq_sizes.(sort)) in
    let add_funcs_in_term =
      T.iter
        (function
        | T.Var _ -> ()
        | T.Func (s, _) ->
            add_func inst s) in

    (* Split literals and create CSP variables for new symbols. *)
    let var_eqs = BatDynArray.create () in
    let var_ineqs = BatDynArray.create () in
    let pos_eq_lits = BatDynArray.create () in
    let neg_eq_lits = BatDynArray.create () in
    let pos_noneq_lits = BatDynArray.create () in
    let neg_noneq_lits = BatDynArray.create () in
    List.iter
      (fun lit -> match%earr lit with
      | L.Lit (sign, s, [| T.Var x; T.Var x' |]) when s = S.sym_eq ->
          BatDynArray.add
            (match sign with
              | Sh.Pos -> var_eqs
              | Sh.Neg -> var_ineqs)
            (x, x')
      | L.Lit (sign, s, [| l; r |]) when s = S.sym_eq ->
          add_funcs_in_term l;
          add_funcs_in_term r;
          BatDynArray.add
            (match sign with
              | Sh.Pos -> pos_eq_lits
              | Sh.Neg -> neg_eq_lits)
            (l, r)
      | L.Lit (sign, s, args) ->
          add_pred inst s;
          Earray.iter add_funcs_in_term args;
          BatDynArray.add
            (match sign with
              | Sh.Pos -> pos_noneq_lits
              | Sh.Neg -> neg_noneq_lits)
            (s, args)
      )
      lits;

    instantiate_clause
      inst
      var_adeq_sizes
      (Earray.of_dyn_array var_eqs)
      (Earray.of_dyn_array var_ineqs)
      (Earray.of_dyn_array pos_eq_lits)
      (Earray.of_dyn_array neg_eq_lits)
      (Earray.of_dyn_array pos_noneq_lits)
      (Earray.of_dyn_array neg_noneq_lits)

  (* Computes domain size of the sort [sort]. *)
  let dsize ~n ~sorts sort =
    let adeq_size = sorts.Sorts.adeq_sizes.(sort) in
    if adeq_size = 0 || adeq_size >= n
    then n
    else adeq_size

  (* LNH for sort [sort]. Assumes that all values in the domain
     of [sort] are unused (i.e. interchangeable).
  *)
  let lnh inst sort =
    let dom_size = dsize ~n:inst.n ~sorts:inst.sorts sort in

    let lower_eq' x c =
      (* Post constraint only when [c] is lower than the maximal element
         (which is [dom_size - 1]) of [sort].
      *)
      if c < dom_size - 1 then
        Solv.lower_eq inst.solver x c in

    let precede' xs cs =
      if xs <> Earray.empty && Earray.length cs >= 2 then
        Solv.precede inst.solver xs cs in

    (* Both [funcs] and [almost_consts] contain function symbols which have
       [sort] as their result sort.

       Additionally function symbols in [funcs] have at least one argument
       of sort [sort] and function symbols in [almost_consts] have
       no argument of sort [sort].

       Function symbols without argument of sort [sort] are treated
       the same way as constants by LNH for sort [sort] -
       this explains the name [almost_consts].
    *)
    let funcs, almost_consts =
      let has_sort_as_result s =
        let sorts = Hashtbl.find inst.sorts.Sorts.symb_sorts s in
        sorts.(Symb.arity s) = sort in
      let has_sort_as_arg s =
        let sorts = Hashtbl.find inst.sorts.Sorts.symb_sorts s in
        let arity = Symb.arity s in
        Earray.existsi (fun i sort' -> sort' = sort && i < arity) sorts in
      inst.funcs
      |> BatHashtbl.keys
      |> BatEnum.filter has_sort_as_result
      |> Earray.of_enum
      |> Earray.partition has_sort_as_arg in
    Earray.sort compare almost_consts;
    Earray.sort compare funcs;

    (* CSP variables of processed cells. *)
    let vars = BatDynArray.create () in

    (* Restrict domain sizes of [almost_consts]. *)
    Earray.iter
      (fun s ->
        let s_vars =
          Hashtbl.find inst.func_arrays s
          |> Earray.enum
          (* Ensures that CSP variables shared by two or more
             cells are processed only once.
          *)
          |> BatEnum.uniq
          |> Earray.of_enum in
        Earray.iter
          (fun var ->
            lower_eq' var (BatDynArray.length vars);
            BatDynArray.add vars var)
          s_vars)
      almost_consts;

    (* Precedence constraint for [almost_consts]. *)
    if almost_consts <> Earray.empty then begin
      let max_el = min (BatDynArray.length vars - 1) (dom_size - 1) in
      precede'
        (Earray.of_dyn_array vars)
        (BatEnum.range ~until:max_el 0 |> Earray.of_enum)
    end;

    if funcs <> Earray.empty then begin
      let z =
        if almost_consts <> Earray.empty
        then 0
        else 1 in
      (* Nothing can be restricted when [max_arg >= dom_size - 2].

         Cell indexed by [max_el - 1 = dom_size - 2] may have [max_el]
         as its value since [max_el] is the lowest unused value.
      *)
      for max_arg = 0 to dom_size - 3 do
        (* Restrict domain sizes of cells which have [max_arg]
           as maximal argument(s) of sort [sort]. Arguments
           of the other sorts can be arbitrary (even bigger than [max_arg]).
        *)
        Earray.iter
          (fun s ->
            let arity = Symb.arity s in
            let dom_sizes = BatMap.find s inst.dom_sizes in

            (* Indices of arguments with and without sort [sort]. *)
            let args_with_sort, args_wo_sort =
              let sorts = Hashtbl.find inst.sorts.Sorts.symb_sorts s in
              Earray.init arity (fun i -> i)
              |> Earray.partition (fun i -> sorts.(i) = sort) in

            let dom_sizes_args_with_sort =
              Earray.map (fun _ -> dom_size) args_with_sort in
            let dom_sizes_args_wo_sort =
              Earray.map (fun i -> dom_sizes.(i)) args_wo_sort in

            (* Ensures that CSP variables shared by two or more
               cells are processed only once.

               This sharing currently happens only due to commutativity
               - if two cells share single CSP variable then
               these cells have same [max_arg] so it's fine
               to empty [used_vars] whenever symbol [s] or [max_arg] changes.
            *)
            let used_vars = ref BatSet.empty in

            (* Assignment of arguments with and without sort [sort]. *)
            let a_with_sort = Earray.map (fun _ -> 0) args_with_sort in
            let a_wo_sort = Earray.map (fun _ -> 0) args_wo_sort in

            (* Assignment of all arguments. *)
            let a = Earray.make arity 0 in

            Assignment.each_me a_with_sort 0 (Earray.length a_with_sort)
              dom_sizes_args_with_sort (max_arg + 1)
              (fun _ ->
                (* Copy assignment of arguments with sort [sort]
                   from [a_with_sort] to [a].
                *)
                Earray.iteri
                  (fun i x -> a.(args_with_sort.(i)) <- x)
                  a_with_sort;

                Assignment.each a_wo_sort 0 (Earray.length a_wo_sort)
                  dom_sizes_args_wo_sort inst.n
                  (fun _ ->
                    (* Copy assignment of arguments without sort [sort]
                       from [a_wo_sort] to [a].
                    *)
                    Earray.iteri
                      (fun i x -> a.(args_wo_sort.(i)) <- x)
                      a_wo_sort;

                    let rank =
                      Earray.fold_lefti
                        (fun rank i x -> rank * dom_sizes.(i) + x)
                        0 a in
                    (* CSP variable which corresponds to cell indexed
                       by assignment [a].
                    *)
                    let var = (Hashtbl.find inst.func_arrays s).(rank) in

                    if not (BatSet.mem var !used_vars) then begin
                      used_vars := BatSet.add var !used_vars;
                      let idx = BatDynArray.length vars in
                      (* If [almost_consts] is nonempty then domain of [var]
                         is limited by [idx].
                         If [almost_consts] is empty then domain of [var]
                         is limited by [idx + 1].
                         The reason is that f(0) with [idx = 0] may be limited
                         by at least 1 (since 0 is used as the argument).
                      *)
                      lower_eq' var (idx + z);
                      BatDynArray.add vars var
                    end)))
          funcs;

        (* Precedence constraint. *)
        let last_idx = BatDynArray.length vars - 1 in
        let max_el = min (last_idx + z) (dom_size - 1) in
        let n_to = max_el in
        (* LNH: Every new cell (i.e. cell with [max_arg] as one
           of its arguments of sort [sort]) can use [max_arg + 1]
           without restrictions but [x > max_arg + 1] can be used only
           if [x - 1] is used by some preceding cell.
        *)
        let n_from = max_arg + 1 in
        precede'
          (Earray.of_dyn_array vars)
          (BatEnum.range ~until:n_to n_from |> Earray.of_enum)
      done
    end

  let use_hints inst =
    let funcs =
      inst.func_arrays
      |> BatHashtbl.enum
      |> Earray.of_enum in
    Earray.sort compare funcs;

    Earray.iter
      (fun (s, vars) ->
        let hints = Symb.hints inst.symbols s in
        List.iter
          (function
          | Symb.Permutation -> Solv.all_different inst.solver vars
          | Symb.Latin_square ->
              (* [s] is binary symbol and both of its arguments have
                 the same sort hence the same size.
              *)
              let n = (BatMap.find s inst.dom_sizes).(0) in
              let xs = Earray.sub vars 0 n in
              (* Rows: i-th row is f(i, ?). *)
              for row = 0 to n - 1 do
                Earray.blit vars (row * n) xs 0 n;
                Solv.all_different inst.solver xs
              done;
              (* Columns: j-th column is f(?, j). *)
              for column = 0 to n - 1 do
                for i = 0 to n - 1 do
                  xs.(i) <- vars.(i * n + column)
                done;
                Solv.all_different inst.solver xs
              done)
          hints)
      funcs

  let order_sorts_for_lnh sorts =
    (* Returns sorts of arguments of functions
       with [sort] as their result sort.
    *)
    let arg_sorts sort =
      let test_result_sort (s, sorts) =
        match Symb.kind s with
          | Symb.Func -> sorts.(Symb.arity s) = sort
          | Symb.Pred -> false in
      sorts.Sorts.symb_sorts
      |> BatHashtbl.enum
      |> BatEnum.filter test_result_sort
      |> BatEnum.map (fun (_, sorts) -> Earray.enum sorts |> BatSet.of_enum)
      |> BatEnum.fold BatSet.union BatSet.empty in
    (* The array of pairs [(sort, blocked)].
       Picking sort [sort] for LNH will block sorts [blocked] from LNH
       since LNH assumes that all values in the domain are unused.
    *)
    let blocked_sorts =
      Earray.mapi
        (* [arg_sorts sort] except [sort] will be blocked from LNH
           if [sort] is picked for LNH.
        *)
        (fun sort _ -> sort, BatSet.remove sort (arg_sorts sort))
        sorts.Sorts.adeq_sizes in
    (* Picks sort which blocks minimal number of other sorts. *)
    let pick_sort blocked_sorts =
      if Earray.is_empty blocked_sorts then
        None
      else begin
        (* [sort] is picked for LNH.
           [blocked] is a set of sorts blocked by picking [sort].
        *)
        let sort, blocked =
          blocked_sorts
          |> Earray.enum
          (* Find sort which blocks minimal number of other sorts. *)
          |> BatEnum.arg_min (fun (_, blocked) -> BatSet.cardinal blocked) in
        (* Remove sorts which can't be picked for LNH. *)
        let blocked_sorts =
          let keep sort' = sort' <> sort && not (BatSet.mem sort' blocked) in
          Earray.filter
            (fun (sort', _) -> keep sort')
            blocked_sorts in
        (* [sort] can't be blocked anymore. *)
        let blocked_sorts =
          Earray.map
            (fun (sort', blocked) -> sort', BatSet.remove sort blocked)
            blocked_sorts in
        Some (sort, blocked_sorts)
      end in
    BatEnum.unfold blocked_sorts pick_sort

  let create ?(nthreads = 1) prob sorts n =
    let prob = Prob.read_only prob in

    let dom_sizes =
      BatHashtbl.fold
        (fun symb sorts' m ->
          let dom_sizes_for_symb = Earray.map (dsize ~n ~sorts) sorts' in
          BatMap.add symb dom_sizes_for_symb m)
        sorts.Sorts.symb_sorts
        BatMap.empty in
    let inst = {
      solver = Solv.create nthreads;
      symbols = prob.Prob.symbols;
      sorts;
      n;
      dom_sizes;
      pred_arrays = Hashtbl.create 20;
      func_arrays = Hashtbl.create 20;
      preds = Hashtbl.create 20;
      funcs = Hashtbl.create 20;
      linear = Hashtbl.create (2 * n * n);
      bool_element = Hashtbl.create (n * n);
      int_element = Hashtbl.create (n * n);
      eq_var_var = Hashtbl.create (n * n);
      eq_var_const = Hashtbl.create (n * n);
      can_construct_model = true;
    } in
    (* Distinct constants. *)
    let distinct_consts = Symb.distinct_consts prob.Prob.symbols in
    BatEnum.iter (add_func inst) (Symb.Set.enum distinct_consts);
    if Symb.Set.is_empty distinct_consts |> not then
      distinct_consts
      |> Symb.Set.enum
      |> Earray.of_enum
      |> Earray.map (fun s -> (Hashtbl.find inst.func_arrays s).(0))
      |> Solv.all_different inst.solver;
    (* Clauses. *)
    BatDynArray.iter
      (fun cl -> each_clause inst cl.Clause2.cl_id cl.Clause2.cl_lits)
      prob.Prob.clauses;
    (* LNH. *)
    sorts
    |> order_sorts_for_lnh
    |> BatEnum.iter (fun sort -> lnh inst sort);
    (* Hints. *)
    use_hints inst;
    inst

  let destroy inst = Solv.destroy inst.solver

  let solve inst =
    let result = Solv.solve inst.solver in
    inst.can_construct_model <- result = Sh.Ltrue;
    result

  let solve_timed inst ms =
    Timer.with_timer ms
      (fun () -> Solv.interrupt inst.solver)
      (fun () -> solve inst)

  let construct_model inst =
    if not inst.can_construct_model then
      failwith "construct_model: no model";

    let symbs = ref Symb.Map.empty in

    let add_symb_model t_value s vars =
      if not (Symb.auxiliary inst.symbols s) then
        let param_sizes =
          Earray.sub (BatMap.find s inst.dom_sizes) 0 (Symb.arity s) in
        let values = Earray.map t_value vars in
        symbs := Symb.Map.add s { Ms_model.param_sizes; values } !symbs in

    (* Predicates. *)
    Hashtbl.iter
      (add_symb_model (Solv.bool_value inst.solver))
      inst.pred_arrays;

    (* Functions. *)
    Hashtbl.iter
      (add_symb_model (Solv.int_value inst.solver))
      inst.func_arrays;

    {
      Ms_model.max_size = inst.n;
      Ms_model.symbs = !symbs;
    }

  let get_solver inst = inst.solver

end
