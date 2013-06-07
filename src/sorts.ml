(* Copyright (c) 2013 Radek Micek *)

open BatPervasives

module T = Term
module C = Clause2


(* ************************************************************************* *)
(* Sort inference *)


type inf_sort = Equiv.item

(* Inferred many-sorted signature.
   The equality is not part of the signature.
*)
type 's inferred = {
  (* Sorts of predicate and function symbols.
     A predicate symbol has [n] sorts and a function symbol has [n+1] sorts
     ([n] sorts for its parameters and [1] sort for its result)
     where [n] is the arity of the symbol.
     Predicate symbols must be disjoint from function symbols.
  *)
  inf_symb_sorts : ('s Symb.id, inf_sort array) Hashtbl.t;
  (* Sorts of variables. *)
  inf_var_sorts : (C.id * Term.var, inf_sort) Hashtbl.t;
  (* Equivalence on sorts. *)
  inf_equiv : Equiv.t;
}

let init_inferred () = {
  inf_symb_sorts = Hashtbl.create 20;
  inf_var_sorts = Hashtbl.create 20;
  inf_equiv = Equiv.create ();
}

(* [update_inferred symdb sorts clause_id lit] updates the inferred many-sorted
   signature [sorts] in a such way that [lit] becomes well-sorted:

   - Adds missing predicate and function symbols to the signature
     (commutative functions and symmetric predicates
     have same sort for both parameters).
   - For each equality unites sorts of its arguments.
   - Unites sorts of each parameter/argument pair
     (except for the equality which works on all sorts).
*)
let update_inferred
    (symdb : 's Symb.db)
    (sorts : 's inferred)
    (clause_id : C.id)
    (lit : 's T.lit)
    : unit =

  let get_arg_sort = function
    | T.Var x ->
        let var = (clause_id, x) in
        if not (Hashtbl.mem sorts.inf_var_sorts var) then begin
          let var_sort = Equiv.add_item sorts.inf_equiv in
          Hashtbl.add sorts.inf_var_sorts var var_sort
        end;
        Hashtbl.find sorts.inf_var_sorts var
    (* s is a function symbol since it is used as an argument
       of a predicate or function symbol.
    *)
    | T.Func (s, args) ->
        assert (Symb.arity symdb s = Array.length args);

        (* no sorts are assigned to s. *)
        if not (Hashtbl.mem sorts.inf_symb_sorts s) then begin
          if Symb.commutative symdb s then
            let par_sort = Equiv.add_item sorts.inf_equiv in
            let res_sort = Equiv.add_item sorts.inf_equiv in
            Hashtbl.add sorts.inf_symb_sorts s [| par_sort; par_sort; res_sort |]
          else
            let par_res_sorts =
              Array.init
                (Array.length args + 1)
                (fun _ -> Equiv.add_item sorts.inf_equiv) in
            Hashtbl.add sorts.inf_symb_sorts s par_res_sorts
        end;

        let func_sorts = Hashtbl.find sorts.inf_symb_sorts s in
        assert (Array.length func_sorts = Array.length args + 1);
        (* Sort of the result. *)
        func_sorts.(Array.length args)
    | T.Neg _ -> failwith "get_arg_sort: negation" in

  let each_subterm = function
    | T.Var _ -> ()
    | T.Neg _ -> ()
    | T.Func (s, [| l; r |]) when s = Symb.sym_eq ->
        Equiv.union sorts.inf_equiv (get_arg_sort l) (get_arg_sort r)
    | T.Func (s, args) ->
        let param_sorts =
          assert (Symb.arity symdb s = Array.length args);

          (* no sorts are assigned to s.
             This means that s is a predicate symbol since it is not used
             as an argument of a predicate or function symbol
             (otherwise sorts would have been assigned to s).
          *)
          if not (Hashtbl.mem sorts.inf_symb_sorts s) then begin
            if Symb.commutative symdb s then
              let par_sort = Equiv.add_item sorts.inf_equiv in
              Hashtbl.add sorts.inf_symb_sorts s [| par_sort; par_sort |]
            else
              let par_sorts =
                Array.map (fun _ -> Equiv.add_item sorts.inf_equiv) args in
              Hashtbl.add sorts.inf_symb_sorts s par_sorts
          end;

          let symb_sorts = Hashtbl.find sorts.inf_symb_sorts s in
          assert (
            (* s is a predicate symbol. *)
            Array.length symb_sorts = Array.length args ||
            (* s is a function symbol since it has a result sort. *)
            Array.length symb_sorts = Array.length args + 1);
          (* Omit potential result sort of a function symbol. *)
          Array.sub symb_sorts 0 (Array.length args) in

        let arg_sorts = Array.map get_arg_sort args in

        BatArray.iter2 (Equiv.union sorts.inf_equiv) param_sorts arg_sorts in

  Term.iter each_subterm lit

(* [merge_sorts_of_constants symdb sorts consts] unites sorts of the given
   constants. Missing constants are added to the signature.
*)
let merge_sorts_of_constants
    (symdb : 's Symb.db)
    (sorts : 's inferred)
    (consts : 's Symb.id BatEnum.t) =

  let get_const_sort c =
    assert (Symb.arity symdb c = 0);

    (* no sorts are assigned to c. *)
    if not (Hashtbl.mem sorts.inf_symb_sorts c) then
      Hashtbl.add sorts.inf_symb_sorts c [| Equiv.add_item sorts.inf_equiv |];

    let const_sorts = Hashtbl.find sorts.inf_symb_sorts c in
    assert (Array.length const_sorts = 1);
    const_sorts.(0) in

  match BatEnum.get consts with
    | None -> ()
    | Some c ->
        let sort = get_const_sort c in
        BatEnum.iter
          (get_const_sort |- Equiv.union sorts.inf_equiv sort)
          consts

type sort_id = int

type 's t = {
  symb_sorts : ('s Symb.id, sort_id array) Hashtbl.t;
  var_sorts : (C.id * Term.var, sort_id) Hashtbl.t;
  adeq_sizes : int array;
  consts : 's Symb.id array array;
  only_consts : bool ref;
}

(* Note: Only a many-sorted signature is computed.
   The other record fields have default values.
*)
let infer_sorts (p : 's Prob.t) : 's t =

  (* Infer sorts. *)

  let inf_sorts = init_inferred () in
  let each_clause (cl : 's C.t) : unit =
    List.iter
      (update_inferred p.Prob.symbols inf_sorts cl.C.cl_id)
      cl.C.cl_lits in
  BatDynArray.iter each_clause p.Prob.clauses;
  merge_sorts_of_constants
    p.Prob.symbols
    inf_sorts
    (p.Prob.distinct_consts |> BatDynArray.enum);

  (* Normalize sort ids. *)

  let sort_ids = Hashtbl.create 20 in
  let get_sort_id item =
    let i = Equiv.find inf_sorts.inf_equiv item in
    if Hashtbl.mem sort_ids i then
      Hashtbl.find sort_ids i
    else
      let id = Hashtbl.length sort_ids in
      let _ = Hashtbl.add sort_ids i id in
      id in

  let symb_sorts =
(*
    BatHashtbl.map
      (fun _ -> Array.map get_sort_id)
      inf_sorts.inf_symb_sorts in
*)
    (* Circumvents bug in BatHashtbl.map which manifests itself
       with OCaml 4.00.
    *)
    let h = Hashtbl.create (Hashtbl.length inf_sorts.inf_symb_sorts) in
    Hashtbl.iter
      (fun k v -> Hashtbl.add h k (Array.map get_sort_id v))
      inf_sorts.inf_symb_sorts;
    h in
  let var_sorts =
(*
    BatHashtbl.map
      (fun _ -> get_sort_id)
      inf_sorts.inf_var_sorts in
*)
    (* Circumvents bug in BatHashtbl.map which manifests itself
       with OCaml 4.00.
    *)
    let h = Hashtbl.create (Hashtbl.length inf_sorts.inf_var_sorts) in
    Hashtbl.iter
      (fun k v -> Hashtbl.add h k (get_sort_id v))
      inf_sorts.inf_var_sorts;
    h in

  (* Build the result. *)

  let nsorts = Hashtbl.length sort_ids in
  {
    symb_sorts;
    var_sorts;
    adeq_sizes = Array.make nsorts 0;
    consts = Array.make nsorts [| |];
    only_consts = ref false;
  }


(* ************************************************************************* *)
(* Detection of adequate sort sizes *)


let compute_sort_sizes prob sorts =
  let nsorts = Array.length sorts.consts in

  let consts = Array.make nsorts [] in
  let only_consts = Array.make nsorts true in

  (*
     Go through [sorts.symb_sorts] and for each sort [A] find:
     - all constants of the sort [A]
     - and whether there is some function symbol with arity > 0
       which has [A] as its result sort.
  *)
  let each_symb s symb_sorts =
    let arity = Symb.arity prob.Prob.symbols s in
    assert (
      Array.length symb_sorts = arity ||
      Array.length symb_sorts = arity + 1);
    let is_func = Array.length symb_sorts = arity + 1 in
    if is_func then begin
      let res_sort = symb_sorts.(arity) in
      if arity > 0 then
        (* Function symbol with arity > 0. *)
        only_consts.(res_sort) <- false
      else
        (* Constant. *)
        consts.(res_sort) <- s :: consts.(res_sort)
    end in
  Hashtbl.iter each_symb sorts.symb_sorts;

  let var_eq = Array.make nsorts false in
  let var_func_eq = Array.make nsorts false in

  (* Go through the clauses and for each sort [A] find:
     - if there is a literal [x = y] where [x] has sort [A]
     - and if there is a literal [x = f] where [x] has sort [A].
  *)
  let each_lit clause_id = function
    | T.Var _ -> failwith "invalid literal"
    (* x = y *)
    | T.Func (s, [| T.Var x; T.Var _ |]) when s = Symb.sym_eq ->
        let var_sort = Hashtbl.find sorts.var_sorts (clause_id, x) in
        var_eq.(var_sort) <- true
    (* x = f *)
    | T.Func (s, [| T.Var x; T.Func (_, _) |])
    | T.Func (s, [| T.Func (_, _); T.Var x |]) when s = Symb.sym_eq ->
        let var_sort = Hashtbl.find sorts.var_sorts (clause_id, x) in
        var_func_eq.(var_sort) <- true
    | _ -> () in
  let each_clause cl =
    List.iter (each_lit cl.C.cl_id) cl.C.cl_lits in
  BatDynArray.iter each_clause prob.Prob.clauses;

  (* Put constants into [sorts] record. *)
  Array.iteri (fun i cs -> sorts.consts.(i) <- Array.of_list cs) consts;

  (* Fill adequate domain sizes. *)
  for i = 0 to nsorts - 1 do
    if only_consts.(i) && not var_eq.(i) then
      let k = Array.length sorts.consts.(i) in
      if var_func_eq.(i) then
        sorts.adeq_sizes.(i) <- k + 1
      else
        (* Adequate domain size is 1 when the sort [i]
           contains no constants.
        *)
        sorts.adeq_sizes.(i) <- max k 1
  done;

  (* [true] iff every sort contains only constants. *)
  sorts.only_consts := BatArray.for_all (fun b -> b) only_consts


(* ************************************************************************* *)


let of_problem prob =
  let sorts = infer_sorts prob in
  compute_sort_sizes prob sorts;
  sorts
