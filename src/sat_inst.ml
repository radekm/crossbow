(* Copyright (c) 2013 Radek Micek *)

module type Solver = sig
  include Sat_solver.S

  val new_false_var : t -> var

  val add_symmetry_clause : t -> lit array -> int -> bool

  val add_at_least_one_val_clause : t -> lit array -> int -> bool

  val add_at_most_one_val_clause : t -> lit array -> bool

  val remove_clauses_with_lit : t -> lit -> unit
end

module type Inst_sig = sig
  type solver

  type 's t

  val create : 's Prob.t -> 's Sorts.t -> 's t

  val incr_max_size : 's t -> unit

  val solve : 's t -> Sh.lbool

  val solve_timed : 's t -> int -> Sh.lbool * bool

  val construct_model : 's t -> 's Ms_model.t

  val block_model : 's t -> 's Ms_model.t -> unit

  val get_solver : 's t -> solver

  val get_max_size : 's t -> int
end

module Make (Solv : Solver) :
  Inst_sig with type solver = Solv.t =
struct
  module L = Lit

  type pvar = Solv.var
  type plit = Solv.lit

  type solver = Solv.t

  type lit = {
    l_sign : Sh.sign;

    (* Propositional variables associated with the symbol. *)
    l_pvars : pvar BatDynArray.t;

    (* Variables used as the arguments and the result (if appropriate). *)
    l_vars : Term.var array;

    (* For ranking. *)
    l_commutative : bool;

    (* For ranking. *)
    l_adeq_sizes : int array;
  }

  type clause = {
    (* Adequate sizes of the domains of the variables in the clause. *)
    var_adeq_sizes : int array;

    (* Equalities of variables. *)
    var_equalities : (Term.var * Term.var) array;

    (* Literals for nullary predicates. *)
    nullary_pred_lits : plit array;

    (* Literals with variables. *)
    lits : lit array;
  }

  type commutative = bool

  type 's t = {
    symred : 's Symred.t;
    solver : Solv.t;
    symbols : 's Symb.db;
    sorts : 's Sorts.t;

    (* Flag whether the LNH is enabled. *)
    mutable lnh : bool;

    (* Propositional variables corresponding to nullary predicates. *)
    nullary_pred_pvars : ('s Symb.id, pvar) Hashtbl.t;

    (* Propositional variables for symbols (except nullary predicates). *)
    pvars : ('s Symb.id, pvar BatDynArray.t) Hashtbl.t;

    (* Except nullary predicates. *)
    adeq_sizes : ('s Symb.id, (int array * commutative)) BatMap.t;

    (* Constants and functions. *)
    funcs : 's Symb.id array;

    clauses : clause array;

    (* Except totality clauses. *)
    max_clause_size : int;

    (* The size of a predicate symbol is its arity,
       the size of a function symbol is its arity + 1.
    *)
    max_symb_size : int;

    (* Minimum max_size (= number of the distinct constants). *)
    min_size : int;

    (* Each totality clause contains this variable (positively),
       so totality clauses can be turned off by setting this variable to true.
    *)
    mutable totality_clauses_switch : pvar option;

    (* Current maximal domain size. *)
    mutable max_size : int;

    (* Cells assigned by symmetry reduction. Each cell is represented
       by a triple (symb_id, rank, max_el).
       Constants have rank = 0 and max_el = -1.
    *)
    assig_by_symred : ('s Symb.id * int * int, unit) Hashtbl.t;

    (* List of the cells assigned by symmetry reduction. Used for LNH. *)
    mutable assig_by_symred_list : ('s Symred.cell * (int * int)) list;

    mutable can_construct_model : bool;
  }

  let (|>) = BatPervasives.(|>)

  let create prob sorts =
    let symred = Symred.create prob sorts in
    let solver = Solv.create () in
    let symbols = prob.Prob.symbols in

    (* Sort to make SAT instantiation deterministic. *)
    let sorted_symb_sorts =
      sorts.Sorts.symb_sorts
      |> BatHashtbl.enum
      |> BatList.of_enum
      |> BatList.sort in

    (* Create propositional variables for nullary predicates. *)
    let nullary_pred_pvars = Hashtbl.create 20 in
    List.iter
      (fun (symb, sorts) ->
        if Array.length sorts = 0 then
          Hashtbl.add nullary_pred_pvars symb (Solv.new_var solver))
      sorted_symb_sorts;

    (* Prepare hashtable with propositional variables of symbols. *)
    let pvars = Hashtbl.create 20 in
    Hashtbl.iter
      (fun symb sorts ->
        if Array.length sorts > 0 then
          Hashtbl.add pvars symb (BatDynArray.create ()))
      sorts.Sorts.symb_sorts;

    let adeq_sizes =
      BatHashtbl.fold
        (fun symb sorts' m ->
          if Array.length sorts' > 0 then
            let adeq_sizes' =
              Array.map (fun sort -> sorts.Sorts.adeq_sizes.(sort)) sorts' in
            let commutative = Symb.commutative prob.Prob.symbols symb in
            BatMap.add symb (adeq_sizes', commutative) m
          else
            m)
        sorts.Sorts.symb_sorts
        BatMap.empty in

    let funcs = BatDynArray.create () in
    List.iter
      (fun (symb, sorts) ->
        if Array.length sorts = Symb.arity symb + 1 then
          BatDynArray.add funcs symb)
      sorted_symb_sorts;

    let each_clause cl =
      let nullary_pred_lits = BatDynArray.create () in
      let var_eqs = BatDynArray.create () in
      let lits = BatDynArray.create () in
      let get_var = function
        | Term.Var x -> x
        | _ -> failwith "expected variable" in
      let each_lit = function
        | L.Lit (sign, s, [| l; r |]) when s = Symb.sym_eq ->
            begin match l, r with
              | Term.Var x, Term.Var y ->
                  if sign <> Sh.Pos then
                    failwith "literal is not flat";
                  BatDynArray.add var_eqs (x, y)
              | Term.Func (f, args), Term.Var res
              | Term.Var res, Term.Func (f, args) ->
                  BatDynArray.add lits
                    {
                      l_sign = sign;
                      l_pvars = Hashtbl.find pvars f;
                      l_vars =
                        Array.init
                          (Array.length args + 1)
                          (fun i ->
                            if i < Array.length args
                            then get_var args.(i)
                            else res);
                      l_commutative = Symb.commutative prob.Prob.symbols f;
                      l_adeq_sizes =
                        Array.map
                          (fun sort -> sorts.Sorts.adeq_sizes.(sort))
                          (Hashtbl.find sorts.Sorts.symb_sorts f);
                    }
              | _, _ -> failwith "literal is not flat"
            end
        | L.Lit (sign, p, args) ->
            (* Nullary predicate. *)
            if args = [| |] then
              let pvar = Hashtbl.find nullary_pred_pvars p in
              let plit = Solv.to_lit sign pvar in
              BatDynArray.add nullary_pred_lits plit
            else
              BatDynArray.add lits
                {
                  l_sign = sign;
                  l_pvars = Hashtbl.find pvars p;
                  l_vars = Array.map get_var args;
                  l_commutative = Symb.commutative prob.Prob.symbols p;
                  l_adeq_sizes =
                    Array.map
                      (fun sort -> sorts.Sorts.adeq_sizes.(sort))
                      (Hashtbl.find sorts.Sorts.symb_sorts p);
                } in
      List.iter each_lit cl.Clause2.cl_lits;
      let _, nvars = Clause.normalize_vars cl.Clause2.cl_lits in
      {
        var_adeq_sizes =
          Array.init nvars
            (fun v ->
              let sort =
                Hashtbl.find
                  sorts.Sorts.var_sorts
                  (cl.Clause2.cl_id, v) in
              sorts.Sorts.adeq_sizes.(sort));
        var_equalities = BatDynArray.to_array var_eqs;
        nullary_pred_lits = BatDynArray.to_array nullary_pred_lits;
        lits = BatDynArray.to_array lits;
      } in
    let clauses = BatDynArray.map each_clause prob.Prob.clauses in

    (* Instantiate clauses without variables. *)
    BatDynArray.keep
      (fun cl ->
        if Array.length cl.var_adeq_sizes = 0 then begin
          ignore (Solv.add_clause
                    solver
                    cl.nullary_pred_lits
                    (Array.length cl.nullary_pred_lits));
          false
        end else
          true)
      clauses;

    (* 2 for "at most one value" clauses. *)
    let max_clause_size =
      BatDynArray.fold_left
        (fun m cl ->
          max m (Array.length cl.lits + Array.length cl.nullary_pred_lits))
        2 clauses in

    let max_symb_size =
      Hashtbl.fold
        (fun _ sorts acc -> max (Array.length sorts) acc)
        sorts.Sorts.symb_sorts
        0 in

    {
      symred;
      solver;
      symbols;
      sorts;
      lnh = true;
      nullary_pred_pvars;
      pvars;
      adeq_sizes;
      funcs = BatDynArray.to_array funcs;
      clauses = BatDynArray.to_array clauses;
      max_clause_size;
      max_symb_size;
      min_size = BatDynArray.length prob.Prob.distinct_consts;
      totality_clauses_switch = None;
      max_size = 0;
      assig_by_symred = Hashtbl.create 50;
      assig_by_symred_list = [];
      can_construct_model = false;
    }

  (* Add propositional variables for predicate and function symbols. *)
  let add_prop_vars inst =
    BatMap.iter
      (fun symb (adeq_sizes, commutative) ->
        let cnt =
          let count =
            if commutative
            then Assignment.count_comm_me
            else Assignment.count_me in
          count 0 (Array.length adeq_sizes) adeq_sizes inst.max_size in
        if cnt > 0 then begin
          let pvars = Hashtbl.find inst.pvars symb in
          BatDynArray.add pvars (Solv.new_var inst.solver);
          for i = 2 to cnt do
            ignore (Solv.new_var inst.solver)
          done
        end)
      inst.adeq_sizes

  let assig_to_pvar a len adeq_sizes rank pvars =
    let r, max_el_idx = rank a 0 len adeq_sizes in
    let pvar = r + BatDynArray.get pvars a.(max_el_idx) in
    pvar

  let symmetry_reduction inst pclause =
    let assigned_cells = Symred.incr_max_size inst.symred in
    List.iter
      (fun ((symb, args), (lo, hi)) ->
        let adeq_sizes, commutative = BatMap.find symb inst.adeq_sizes in
        let pvars = Hashtbl.find inst.pvars symb in
        let arity = Array.length args in
        let rank =
          if commutative
          then Assignment.rank_comm_me
          else Assignment.rank_me in
        let a = Array.copy adeq_sizes in
        Array.blit args 0 a 0 arity;
        (* Create literals. *)
        for result = lo to hi do
          a.(arity) <- result;
          let r, max_el_idx = rank a 0 (arity+1) adeq_sizes in
          let pvar = r + BatDynArray.get pvars a.(max_el_idx) in
          let plit = Solv.to_lit Sh.Pos pvar in
          pclause.(result - lo) <- plit
        done;
        ignore (Solv.add_symmetry_clause inst.solver pclause (hi - lo + 1));

        if inst.lnh then begin
          let constrs =
            Lnh.lnh
              inst.symbols inst.sorts
              inst.assig_by_symred_list ((symb, args), (lo, hi)) in
          let cell_to_pvar (symb, args) res =
            let adeq_sizes, commutative = BatMap.find symb inst.adeq_sizes in
            let pvars = Hashtbl.find inst.pvars symb in
            let arity = Array.length args in
            let rank =
              if commutative
              then Assignment.rank_comm_me
              else Assignment.rank_me in
            let a =
              Array.init
                (arity+1)
                (fun i -> if i < arity then args.(i) else res) in
            let r, max_el_idx = rank a 0 (arity+1) adeq_sizes in
            let pvar = r + BatDynArray.get pvars a.(max_el_idx) in
            pvar in
          (* Add constraints generated by LNH. *)
          Array.iter
            (fun { Lnh.assig = (c, v); Lnh.required = (cs, v') } ->
              let pclause =
                Array.init
                  (Array.length cs + 1)
                  (fun i ->
                    if i < Array.length cs then
                      let pvar = cell_to_pvar cs.(i) v' in
                      let plit = Solv.to_lit Sh.Pos pvar in
                      plit
                    else
                      let pvar = cell_to_pvar c v in
                      let plit = Solv.to_lit Sh.Neg pvar in
                      plit) in
              ignore (Solv.add_clause inst.solver pclause
                        (Array.length pclause)))
            constrs
        end;

        (* Mark cell as assigned by symmetry reduction. *)
        let cell = ((symb, args), (lo, hi)) in
        inst.assig_by_symred_list <- cell :: inst.assig_by_symred_list;

        (* Mark cell as assigned by symmetry reduction. *)
        if Array.length args = 0 then
          Hashtbl.add inst.assig_by_symred (symb, 0, -1) ()
        else
          let r, max_el_idx = rank args 0 (Array.length args) adeq_sizes in
          Hashtbl.add inst.assig_by_symred (symb, r, args.(max_el_idx)) ()
      )
      assigned_cells

  let ban_values_eliminated_by_symmetry_reduction inst =
    List.iter
      (fun ((symb, args), (lo, hi)) ->
        let adeq_sizes, commutative = BatMap.find symb inst.adeq_sizes in
        let pvars = Hashtbl.find inst.pvars symb in
        let arity = Array.length args in
        let res_max_el =
          if
            adeq_sizes.(arity) = 0 ||
            adeq_sizes.(arity) >= inst.max_size
          then inst.max_size - 1
          else adeq_sizes.(arity) - 1 in
        let rank =
          if commutative
          then Assignment.rank_comm_me
          else Assignment.rank_me in
        let a = Array.copy adeq_sizes in
        Array.blit args 0 a 0 arity;
        (* Explicitly ban values which are not between lo and hi. *)
        for result = 0 to res_max_el do
          if result < lo || result > hi then begin
            a.(arity) <- result;
            let r, max_el_idx = rank a 0 (arity+1) adeq_sizes in
            let pvar = r + BatDynArray.get pvars a.(max_el_idx) in
            let plit = Solv.to_lit Sh.Neg pvar in
            ignore (Solv.add_clause inst.solver [| plit |] 1)
          end
        done)
      inst.assig_by_symred_list

  let add_at_most_one_val_clauses inst pclause =
    Array.iter
      (fun f ->
        let adeq_sizes, commutative = BatMap.find f inst.adeq_sizes in
        let pvars = Hashtbl.find inst.pvars f in
        let arity = Array.length adeq_sizes - 1 in
        let res_max_el =
          if
            adeq_sizes.(arity) = 0 ||
            adeq_sizes.(arity) >= inst.max_size
          then inst.max_size - 1
          else adeq_sizes.(arity) - 1 in
        let each, rank =
          if commutative
          then Assignment.each_comm_me, Assignment.rank_comm_me
          else Assignment.each_me, Assignment.rank_me in
        let a = Array.copy adeq_sizes in
        let mk_lit a =
          let r, max_el_idx = rank a 0 (arity+1) adeq_sizes in
          let pvar = r + BatDynArray.get pvars a.(max_el_idx) in
          Solv.to_lit Sh.Neg pvar in

        (* Points without maximal element. *)
        if res_max_el = inst.max_size - 1 then
          let proc_arg_vec a =
            (* The first result is always the maximal element. *)
            a.(arity) <- res_max_el;
            pclause.(0) <- mk_lit a;
            (* The second result. *)
            for result = 0 to res_max_el - 1 do
              a.(arity) <- result;
              pclause.(1) <- mk_lit a;
              ignore (Solv.add_at_most_one_val_clause inst.solver pclause)
            done in
          (* Constants are processed separately since both each_me
             and each_comm_me don't produce any assignment.
          *)
          if arity = 0 then
            proc_arg_vec a
          else
            for max_size = 1 to inst.max_size - 1 do
              each a 0 arity adeq_sizes max_size proc_arg_vec
            done;
        (* Points with maximal element. *)
        each a 0 arity adeq_sizes inst.max_size
          (fun a ->
            for result = 0 to res_max_el - 1 do
              a.(arity) <- result;
              pclause.(0) <- mk_lit a;
              for result2 = result + 1 to res_max_el do
                a.(arity) <- result2;
                pclause.(1) <- mk_lit a;
                ignore (Solv.add_at_most_one_val_clause inst.solver pclause)
              done
            done))
      inst.funcs

  let instantiate_clauses inst pclause =
    (* Array where the arguments and the result (if appropriate) are stored
       before they are ranked.
    *)
    let symb_elems = Array.make inst.max_symb_size 0 in

    Array.iter
      (fun cl ->
        let nullary_preds_cnt = Array.length cl.nullary_pred_lits in
        Array.blit cl.nullary_pred_lits 0 pclause 0 nullary_preds_cnt;
        let a = Array.copy cl.var_adeq_sizes in
        (* For each assignment of the variables with max_el. *)
        Assignment.each_me
          a 0 (Array.length cl.var_adeq_sizes)
          cl.var_adeq_sizes inst.max_size
          (fun a ->
            let var_eq_sat =
              BatArray.exists
                (fun (x, y) -> a.(x) = a.(y))
                cl.var_equalities in
            if not var_eq_sat then begin
              (* Add remaining literals. *)
              Array.iteri
                (fun i lit ->
                  (* Copy values of variables into symb_elems. *)
                  Array.iteri
                    (fun j x -> symb_elems.(j) <- a.(x))
                    lit.l_vars;
                  let rank =
                    if lit.l_commutative
                    then Assignment.rank_comm_me
                    else Assignment.rank_me in
                  let r, max_el_idx =
                    rank
                      symb_elems 0 (Array.length lit.l_vars)
                      lit.l_adeq_sizes in
                  let pvar =
                    r + BatDynArray.get lit.l_pvars symb_elems.(max_el_idx) in
                  let plit = Solv.to_lit lit.l_sign pvar in
                  pclause.(i + nullary_preds_cnt) <- plit)
                cl.lits;
              ignore
                (Solv.add_clause
                   inst.solver pclause
                   (nullary_preds_cnt + Array.length cl.lits))
            end))
      inst.clauses

  let incr_max_size inst =
    inst.max_size <- inst.max_size + 1;
    inst.can_construct_model <- false;

    add_prop_vars inst;

    (* Disable old "at least one value" clauses. *)
    begin match inst.totality_clauses_switch with
      | None -> ()
      | Some pvar ->
          let plit = Solv.to_lit Sh.Pos pvar in
          Solv.remove_clauses_with_lit inst.solver plit;
          inst.totality_clauses_switch <- None;
    end;

    (* Array where the propositional literals are stored
       before they are added as a new clause.
    *)
    let pclause =
      Array.make
        (* inst.max_size is for symmetry clauses. *)
        (max inst.max_clause_size inst.max_size)
        (Solv.to_lit Sh.Pos 0) in

    symmetry_reduction inst pclause;
    add_at_most_one_val_clauses inst pclause;
    instantiate_clauses inst pclause

  let add_at_least_one_val_clauses inst =
    if inst.totality_clauses_switch = None then begin
      let switch = Solv.new_false_var inst.solver in
      inst.totality_clauses_switch <- Some switch;

      let pclause =
        Array.make
          (* + 1 is for the switch. *)
          (inst.max_size + 1)
          (Solv.to_lit Sh.Pos 0) in

      Array.iter
        (fun f ->
          let adeq_sizes, commutative = BatMap.find f inst.adeq_sizes in
          let pvars = Hashtbl.find inst.pvars f in
          let arity = Array.length adeq_sizes - 1 in
          let res_max_el =
            if
              adeq_sizes.(arity) = 0 ||
              adeq_sizes.(arity) >= inst.max_size
            then inst.max_size - 1
            else adeq_sizes.(arity) - 1 in
          let each, rank =
            if commutative
            then Assignment.each_comm_me, Assignment.rank_comm_me
            else Assignment.each_me, Assignment.rank_me in
          (* Process argument vector. *)
          let proc_arg_vec a =
            let assig_by_symred =
              if arity = 0 then
                Hashtbl.mem inst.assig_by_symred (f, 0, -1)
              else
                let r, max_el_idx = rank a 0 arity adeq_sizes in
                Hashtbl.mem inst.assig_by_symred (f, r, a.(max_el_idx)) in
            (* Skip cells assigned by symmetry reduction. *)
            if not assig_by_symred then begin
              for result = 0 to res_max_el  do
                a.(arity) <- result;
                let r, max_el_idx = rank a 0 (arity + 1) adeq_sizes in
                let pvar = r + BatDynArray.get pvars a.(max_el_idx) in
                let plit = Solv.to_lit Sh.Pos pvar in
                pclause.(result) <- plit
              done;
              pclause.(res_max_el + 1) <- Solv.to_lit Sh.Pos switch;
              ignore (Solv.add_at_least_one_val_clause
                        inst.solver
                        pclause
                        (res_max_el + 2))
            end in

          let a = Array.copy adeq_sizes in

          (* Constants are processed separately since both each_me
             and each_comm_me don't produce any assignment.
          *)
          if arity = 0 then
            proc_arg_vec a
          else
            for max_size = 1 to inst.max_size do
              each a 0 arity adeq_sizes max_size proc_arg_vec
            done)
        inst.funcs
    end

  let solve inst =
    if inst.max_size < 1 then
      failwith "solve: max_size must be at least 1";
    if inst.max_size < inst.min_size then
      failwith "solve: max_size is too small";
    ban_values_eliminated_by_symmetry_reduction inst;
    add_at_least_one_val_clauses inst;
    match inst.totality_clauses_switch with
      | None -> failwith "solve: impossible"
      | Some switch ->
          let result =
            Solv.solve inst.solver [| Solv.to_lit Sh.Neg switch |] in
          inst.can_construct_model <- result = Sh.Ltrue;
          result

  let solve_timed inst ms =
    Timer.with_timer ms
      (fun () -> Solv.interrupt inst.solver)
      (fun () -> solve inst)

  let construct_model inst =
    if not inst.can_construct_model then
      failwith "construct_model: no model";

    let get_val pvar =
      match Solv.model_value inst.solver pvar with
        | Sh.Ltrue -> 1
        | Sh.Lfalse -> 0
        | Sh.Lundef ->
            failwith "construct_model: unassigned propositional variable" in

    let symbs = ref Symb.Map.empty in

    (* Nullary predicates. *)
    Hashtbl.iter
      (fun s pvar ->
        if not (Symb.auxiliary inst.symbols s) then
          symbs :=
            Symb.Map.add s
              {
                Ms_model.param_sizes = [| |];
                Ms_model.values = [| get_val pvar |];
              }
              !symbs)
      inst.nullary_pred_pvars;

    (* Functions, constants, non-nullary predicates. *)
    BatMap.iter
      (fun s (adeq_sizes, commutative) ->
        let arity = Symb.arity s in
        if not (Symb.auxiliary inst.symbols s) || arity = 0 then begin
          let dsize i =
            if
              adeq_sizes.(i) = 0 ||
              adeq_sizes.(i) >= inst.max_size
            then inst.max_size
            else adeq_sizes.(i) in
          let i = ref 0 in
          let values =
            Array.make
              (Assignment.count 0 arity adeq_sizes inst.max_size)
              ~-1 in
          symbs :=
            Symb.Map.add s
              {
                Ms_model.param_sizes = Array.init arity dsize;
                Ms_model.values;
              }
              !symbs;
          let pvars = Hashtbl.find inst.pvars s in
          let a = Array.copy adeq_sizes in
          let rank =
            if commutative
            then Assignment.rank_comm_me
            else Assignment.rank_me in
          if arity = Array.length adeq_sizes then
            (* Non-nullary predicate. *)
            Assignment.each a 0 arity adeq_sizes inst.max_size
              (fun a ->
                let r, max_el_idx = rank a 0 arity adeq_sizes in
                let pvar = r + BatDynArray.get pvars a.(max_el_idx) in
                values.(!i) <- get_val pvar;
                incr i)
          else
            (* Function or constant. *)
            let res_max_el = dsize arity - 1 in
            Assignment.each a 0 arity adeq_sizes inst.max_size
              (fun a ->
                let result = ref ~-1 in
                (* Optimization: The loop can be interrupted
                   when the result is found.
                *)
                for res = 0 to res_max_el do
                  a.(arity) <- res;
                  let r, max_el_idx = rank a 0 (arity+1) adeq_sizes in
                  let pvar = r + BatDynArray.get pvars a.(max_el_idx) in
                  if get_val pvar = 1 then
                    if !result = ~-1 then
                      result := res
                    else
                      failwith "construct_model: function with more values"
                done;
                if !result = ~- 1 then
                  failwith "construct_model: function with no value";
                values.(!i) <- !result;
                incr i)
        end)
      inst.adeq_sizes;

    {
      Ms_model.max_size = inst.max_size;
      Ms_model.symbs = !symbs;
    }

  let block_model inst model =
    if model.Ms_model.max_size > inst.max_size then
      (* Some propositional variables may not exist. *)
      failwith "block_model: max_size";

    let pclause = BatDynArray.create () in

    (* Construct clause which blocks current model. *)
    Symb.Map.iter
      (fun s table ->
        match Symb.arity s, Symb.kind s with
          (* Nullary predicate. *)
          | 0, Symb.Pred ->
              let pvar = Hashtbl.find inst.nullary_pred_pvars s in
              let sign =
                if table.Ms_model.values.(0) = 0
                then Sh.Pos
                else Sh.Neg in
              let plit = Solv.to_lit sign pvar in
              BatDynArray.add pclause plit
          (* Function or non-nullary predicate. *)
          | arity, kind ->
              let adeq_sizes, commutative = BatMap.find s inst.adeq_sizes in
              let pvars = Hashtbl.find inst.pvars s in
              let rank =
                if commutative
                then Assignment.rank_comm_me
                else Assignment.rank_me in
              let a = Array.copy adeq_sizes in
              let i = ref 0 in
              (* Note: Commutative symbols are not treated specially
                 when generating argument vectors. The consequence
                 is that pclause may contain duplicate literals.
              *)
              begin match kind with
                | Symb.Pred ->
                    Assignment.each a 0 arity adeq_sizes inst.max_size
                      (fun a ->
                        let pvar =
                          assig_to_pvar a arity adeq_sizes rank pvars in
                        let sign =
                          if table.Ms_model.values.(!i) = 0
                          then Sh.Pos
                          else Sh.Neg in
                        let plit = Solv.to_lit sign pvar in
                        BatDynArray.add pclause plit;
                        incr i)
                | Symb.Func ->
                    Assignment.each a 0 arity adeq_sizes inst.max_size
                      (fun a ->
                        (* Fetch value from the model. *)
                        a.(arity) <- table.Ms_model.values.(!i);
                        let pvar =
                          assig_to_pvar a (arity+1) adeq_sizes rank pvars in
                        let plit = Solv.to_lit Sh.Neg pvar in
                        BatDynArray.add pclause plit;
                        incr i)
              end)
      model.Ms_model.symbs;

    let pclause = BatDynArray.to_array pclause in
    ignore (Solv.add_clause inst.solver pclause (Array.length pclause))

  let get_solver inst = inst.solver

  let get_max_size inst = inst.max_size

end
