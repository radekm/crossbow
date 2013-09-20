(* Copyright (c) 2013 Radek Micek *)

module type Inst_sig = sig
  type solver
  type 's t

  val create : ?nthreads:int -> 's Prob.t -> int -> 's t

  val solve : 's t -> Sh.lbool

  val solve_timed : 's t -> int -> Sh.lbool * bool

  val construct_model : 's t -> 's Model.t

  val get_solver : 's t -> solver
end

module Make (Solv : Csp_solver.S) :
  Inst_sig with type solver = Solv.t =
struct
  type solver = Solv.t

  module S = Symb
  module T = Term
  module L = Lit

  let (|>) = BatPervasives.(|>)

  type 's t = {
    solver : Solv.t;
    symbols : 's Symb.db;

    (* Domain size. *)
    n : int;

    (* Variables for predicates. *)
    pred_arrays : ('s S.id, bool Solv.var array) Hashtbl.t;

    (* Variables for functions. *)
    func_arrays : ('s S.id, int Solv.var array) Hashtbl.t;

    (* Arrays of CSP variables for bool_element constraint. *)
    preds : ('s S.id, bool Solv.var_array) Hashtbl.t;

    (* Arrays of CSP variables for int_element constraint. *)
    funcs : ('s S.id, int Solv.var_array) Hashtbl.t;

    (* Maps the result of x1 * c1 + x2 * c2 + x3 * c3 + ... + c to variable y.
       Variables xi in polynomial are sorted and unique.
    *)
    linear : ((int Solv.var * int) array * int, int Solv.var) Hashtbl.t;

    bool_element : ('s S.id * int Solv.var, bool Solv.var) Hashtbl.t;

    int_element : ('s S.id * int Solv.var, int Solv.var) Hashtbl.t;

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

  (* Index into the array representing a symbol table.
     a is assignment.
  *)
  let rec index
      (inst : 's t)
      (a : int array)
      (args : 's T.t array) : index =

    (* Index can be computed statically. *)
    if BatArray.for_all T.is_var args then
      I_const (Array.fold_left (fun i x -> i * inst.n + a.(get_var x)) 0 args)
    else begin
      (* Maps CSP variables to their coefficients. *)
      let vars = ref BatMap.empty in
      let c, _ =
        Array.fold_right
          (fun arg (c, mult) ->
            match arg with
              | T.Var x -> (c + a.(x) * mult, mult * inst.n)
              | T.Func (s, _) ->
                  (* Get CSP variable for each argument
                     which is function term.
                  *)
                  let y = var_for_func_term inst a arg in
                  vars :=
                    BatMap.modify_def 0 y (fun coef -> coef + mult) !vars;
                  (c, mult * inst.n))
          args (0, 1) in
      let vars = BatMap.enum !vars |> BatArray.of_enum in
      let nvars = Array.length vars in
      match vars with
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
                  let max_idx =
                    Array.fold_left
                      (fun max_idx (_, coef) -> max_idx + (inst.n - 1) * coef)
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
                    (Array.init (nvars + 1)
                       (fun i -> if i < nvars then fst vars.(i) else y))
                    (Array.init (nvars + 1)
                       (fun i -> if i < nvars then snd vars.(i) else ~-1))
                    ~-c;
                  Hashtbl.add inst.linear key y;
                  I_var y
    end

  (* a is assignment. *)
  and var_for_func_term
      (inst : 's t)
      (a : int array) : 's T.t -> int Solv.var = function

    | T.Var _ -> failwith "var_for_func_term"
    | T.Func (s, args) ->
        match index inst a args with
          | I_const i ->
              (Hashtbl.find inst.func_arrays s).(i)
          | I_var i ->
              let key = (s, i) in
              (* Variable satisfying int_element constraint. *)
              try
                Hashtbl.find inst.int_element key
              with
                | Not_found ->
                    let y = Solv.new_tmp_int_var inst.solver inst.n in
                    Solv.int_element
                      inst.solver
                      (Hashtbl.find inst.funcs s) i y;
                    Hashtbl.add inst.int_element key y;
                    y

  (* a is assignment. *)
  let var_for_noneq_atom
      (inst : 's t)
      (a : int array)
      (s : 's S.id)
      (args : 's T.t array) : bool Solv.var =

    assert (s <> S.sym_eq);
    match index inst a args with
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

  (* a is assignment. *)
  let var_for_eq_atom
      (inst : 's t)
      (a : int array)
      (l : 's T.t)
      (r : 's T.t) : bool Solv.var =

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

  let instantiate_clause
      (inst : 's t)
      (nvars : int)
      (var_eqs : (T.var * T.var) array)
      (var_ineqs : (T.var * T.var) array)
      (pos_eq_lits : ('s T.t * 's T.t) array)
      (neg_eq_lits : ('s T.t * 's T.t) array)
      (pos_noneq_lits : ('s Symb.id * 's T.t array) array)
      (neg_noneq_lits : ('s Symb.id * 's T.t array) array) : unit =

    (* Arrays for literals. *)
    let pos =
      Array.make
        (Array.length pos_eq_lits + Array.length pos_noneq_lits)
        dummy_bool_var in
    let neg =
      Array.make
        (Array.length neg_eq_lits + Array.length neg_noneq_lits)
        dummy_bool_var in

    Assignment.each (Array.make nvars 0) 0 nvars (Array.make nvars 0) inst.n
      (fun a ->
        (* If no (in)equality of variables is satisfied. *)
        if
          BatArray.for_all (fun (x, x') -> a.(x) <> a.(x')) var_eqs &&
          BatArray.for_all (fun (x, x') -> a.(x) = a.(x')) var_ineqs
        then begin
          (* Positive literals. *)
          Array.iteri
            (fun i (l, r) -> pos.(i) <- var_for_eq_atom inst a l r)
            pos_eq_lits;
          let skip = Array.length pos_eq_lits in
          Array.iteri
            (fun i (s, args) ->
              pos.(skip + i) <- var_for_noneq_atom inst a s args)
            pos_noneq_lits;

          (* Negative literals. *)
          Array.iteri
            (fun i (l, r) -> neg.(i) <- var_for_eq_atom inst a l r)
            neg_eq_lits;
          let skip = Array.length neg_eq_lits in
          Array.iteri
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
      (commutative : bool)
      (new_t_var : Solv.t -> 'a Solv.var)
      (new_t_var_array : Solv.t -> 'a Solv.var array -> 'a Solv.var_array) =

    let adeq_sizes = Array.make arity 0 in
    if commutative then begin
      let count = ref 0 in
      let starts = Array.make n 0 in
      for max_size = 1 to n do
        starts.(max_size-1) <- !count;
        count := !count + Assignment.count_comm_me 0 arity adeq_sizes max_size
      done;
      let distinct_vars = Array.init !count (fun _ -> new_t_var solver) in
      let vars = BatDynArray.create () in
      Assignment.each (Array.make arity 0) 0 arity adeq_sizes n
        (fun a ->
          let r, max_el_idx = Assignment.rank_comm_me a 0 arity adeq_sizes in
          let var_idx = r + starts.(a.(max_el_idx)) in
          let var = distinct_vars.(var_idx) in
          BatDynArray.add vars var);
      let vars = BatDynArray.to_array vars in
      (new_t_var_array solver vars, vars)
    end else begin
      let count = Assignment.count 0 arity adeq_sizes n in
      let vars = Array.init count (fun _ -> new_t_var solver) in
      (new_t_var_array solver vars, vars)
    end

  let add_pred inst s =
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
          (Symb.commutative inst.symbols s)
          new_var
          Solv.new_bool_var_array in
      Hashtbl.add inst.pred_arrays s arr;
      Hashtbl.add inst.preds s vars
    end

  let add_func inst s =
    if not (Hashtbl.mem inst.funcs s) then begin
      let new_var =
        if Symb.auxiliary inst.symbols s
        then Solv.new_tmp_int_var
        else Solv.new_int_var in
      let vars, arr =
        add_symb
          inst.solver
          inst.n
          (Symb.arity s)
          (Symb.commutative inst.symbols s)
          (fun solver -> new_var solver inst.n)
          Solv.new_int_var_array in
      Hashtbl.add inst.func_arrays s arr;
      Hashtbl.add inst.funcs s vars
    end

  let each_clause inst lits =
    let nvars = Sh.IntSet.cardinal (Clause.vars lits) in

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
      (function
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
          Array.iter add_funcs_in_term args;
          BatDynArray.add
            (match sign with
              | Sh.Pos -> pos_noneq_lits
              | Sh.Neg -> neg_noneq_lits)
            (s, args)
      )
      lits;

    instantiate_clause
      inst
      nvars
      (BatDynArray.to_array var_eqs)
      (BatDynArray.to_array var_ineqs)
      (BatDynArray.to_array pos_eq_lits)
      (BatDynArray.to_array neg_eq_lits)
      (BatDynArray.to_array pos_noneq_lits)
      (BatDynArray.to_array neg_noneq_lits)

  let lnh inst =
    let lower_eq' x c =
      if c < inst.n - 1 then
        Solv.lower_eq inst.solver x c in

    let precede' xs cs =
      if xs <> [| |] && Array.length cs >= 2 then
        Solv.precede inst.solver xs cs in

    let consts, funcs =
      inst.func_arrays
      |> BatHashtbl.keys
      |> BatArray.of_enum
      |> BatArray.partition (fun s -> Symb.arity s = 0) in
    Array.sort compare consts;
    Array.sort compare funcs;

    let nconsts = Array.length consts in

    (* CSP variables for processed cells. *)
    let vars = BatDynArray.create () in

    (* Restrict domain sizes of constants. *)
    Array.iteri
      (fun i c ->
        let var = (Hashtbl.find inst.func_arrays c).(0) in
        (* Index of var in array vars. *)
        let idx = i in
        lower_eq' var idx;
        BatDynArray.add vars var)
      consts;

    (* Precedence constraint for constants. *)
    if consts <> [| |] then begin
      let max_el = min (nconsts - 1) (inst.n - 1) in
      precede'
        (BatDynArray.to_array vars)
        (BatEnum.range ~until:max_el 0 |> BatArray.of_enum)
    end;

    if funcs <> [| |] then begin
      let z =
        if consts <> [| |]
        then 0
        else 1 in
      (* Nothing can be restricted when max_arg = inst.n - 1. *)
      for max_arg = 0 to inst.n - 2 do
        (* Restrict domain sizes of cells with maximal argument max_arg. *)
        Array.iter
          (fun f ->
            let arity = Symb.arity f in
            let each =
              if Symb.commutative inst.symbols f
              then Assignment.each_comm_me
              else Assignment.each_me in
            let a = Array.make arity 0 in
            let adeq_sizes = Array.make arity 0 in
            each a 0 arity adeq_sizes (max_arg + 1)
              (fun a ->
                let rank =
                  Array.fold_left (fun rank x -> rank * inst.n + x) 0 a in
                let var = (Hashtbl.find inst.func_arrays f).(rank) in
                (* Index of var in array vars. *)
                let idx = BatDynArray.length vars in
                (* If there is some constant then domain of var
                   is limited by idx.
                   If there is no constant then domain of var
                   is limited by idx + 1.
                   The reason is that f(0) with idx = 0 may be limited
                   by at least 1 (since 0 is used as the argument).
                *)
                lower_eq' var (idx + z);
                BatDynArray.add vars var))
          funcs;
        (* Precedence constraint. *)
        let last_idx = BatDynArray.length vars - 1 in
        let max_el = min (last_idx + z) (inst.n - 1) in
        let n_to = max_el in
        (* Every new cell can use max_arg + 1 without restrictions. *)
        let n_from = max_arg + 1 in
        precede'
          (BatDynArray.to_array vars)
          (BatEnum.range ~until:n_to n_from |> BatArray.of_enum)
      done
    end

  let use_hints inst =
    let funcs =
      inst.func_arrays
      |> BatHashtbl.enum
      |> BatArray.of_enum in
    Array.sort compare funcs;

    Array.iter
      (fun (s, vars) ->
        let hints = Symb.hints inst.symbols s in
        List.iter
          (function
          | Symb.Permutation -> Solv.all_different inst.solver vars
          | Symb.Latin_square ->
              let n = inst.n in
              let xs = Array.sub vars 0 n in
              (* Rows: i-th row is f(i, ?). *)
              for row = 0 to n - 1 do
                Array.blit vars (row * n) xs 0 n;
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

  let create ?(nthreads = 1) prob n =
    let inst = {
      solver = Solv.create nthreads;
      symbols = prob.Prob.symbols;
      n;
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
    BatDynArray.iter (add_func inst) prob.Prob.distinct_consts;
    if BatDynArray.empty prob.Prob.distinct_consts |> not then
      BatDynArray.to_array prob.Prob.distinct_consts
      |> Array.map (fun s -> (Hashtbl.find inst.func_arrays s).(0))
      |> Solv.all_different inst.solver;
    (* Clauses. *)
    BatDynArray.iter
      (fun cl -> each_clause inst cl.Clause2.cl_lits)
      prob.Prob.clauses;
    (* LNH. *)
    lnh inst;
    (* Hints. *)
    use_hints inst;
    inst

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
        let values = Array.map t_value vars in
        symbs := Symb.Map.add s { Model.values } !symbs in

    (* Predicates. *)
    Hashtbl.iter
      (add_symb_model (Solv.bool_value inst.solver))
      inst.pred_arrays;

    (* Functions. *)
    Hashtbl.iter
      (add_symb_model (Solv.int_value inst.solver))
      inst.func_arrays;

    {
      Model.max_size = inst.n;
      Model.symbs = !symbs;
    }

  let get_solver inst = inst.solver

end
