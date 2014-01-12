(* Copyright (c) 2013 Radek Micek *)

module S = Symb
module T = Term
module L = Lit

type t = L.t list

let (|>) = BatPervasives.(|>)
let (%>) = BatPervasives.(%>)

(* Every inequality of variables [x != y] is removed and [x]
   is replaced by [y].
*)
let remove_var_ineqs lits =
  let var_ineq lit = match%earr lit with
    | L.Lit (Sh.Neg, s, [| (T.Var _) as x; (T.Var _) as y |])
      when s = S.sym_eq ->
        Some (x, y)
    | _ -> None in
  let rec loop lits =
    match Elist.pick_and_remove var_ineq lits with
      | None, _ -> lits
      | Some (x, y), lits ->
          lits
          |> BatList.map (L.replace x y)
          |> loop in
  loop lits

let simplify symdb lits =
  let lits =
    lits
    |> remove_var_ineqs
    |> BatList.map (L.normalize_comm symdb)
    |> BatList.unique
    |> BatList.filter (L.is_false %> not) in
  let taut =
    List.exists
      (fun l -> L.is_true l || Elist.contains (L.neg l) lits)
      lits in
  if taut then None else Some lits

let simplify_all symdb clauses =
  let clauses' = BatDynArray.filter_map (simplify symdb) clauses in
  if
    clauses'
    |> BatDynArray.enum
    |> BatEnum.exists ((=) [])
  then
    BatDynArray.of_array [| [] |]
  else
    clauses'

let normalize_vars lits =
  let vars = Hashtbl.create 20 in
  let norm_var x =
    if Hashtbl.mem vars x then
      Hashtbl.find vars x
    else
      let y = Hashtbl.length vars in
      let _ = Hashtbl.add vars x y in
      y in
  let rec norm_vars_t = function
    | T.Var x -> T.var (norm_var x)
    | T.Func (s, args) -> T.func (s, Earray.map norm_vars_t args) in
  let norm_vars_l = L.lift norm_vars_t in
  let lits2 = BatList.map norm_vars_l lits in
  lits2, Hashtbl.length vars

let rewrite_ground_terms symdb clauses =
  let rec loop clauses =
    (* Oriented equality of ground terms which can be used
       for rewriting (i.e. right term has occurence
       in another clause).
    *)
    let equality =
      (* If the clause is ground equality orient it,
         otherwise return None.
      *)
      let orient_ground_eq a = match%earr a with
        | i, [L.Lit (Sh.Pos, s, [| l; r |])]
          when s = Symb.sym_eq && T.is_ground l && T.is_ground r ->
            let nl, nr = T.count_symbs l, T.count_symbs r in
            let eq =
              (* Simpler term to the left. *)
              if nl < nr then (l, r)
              else if nl > nr then (r, l)
              (* Clauses are simplified so l <> r
                 (l = r would cause non-termination).
              *)
              else if l <= r then (l, r)
              else (r, l) in
            Some (i, eq)
        | _ -> None in
      (* Right term has occurence in another clause. *)
      let has_occurence (i, eq) =
        let _, r = eq in
        let cond (j, cl) =
          j <> i && List.exists (L.contains r) cl in
        clauses
        |> BatDynArray.enum
        |> BatEnum.mapi (fun j cl -> (j, cl))
        |> BatEnum.exists cond in
      clauses
      |> BatDynArray.enum
      |> BatEnum.mapi (fun i cl -> (i, cl))
      |> BatEnum.filter_map orient_ground_eq
      |> BatEnum.filter has_occurence
      |> BatEnum.peek in

    match equality with
      | None -> clauses
      | Some (i, (l, r)) ->
          let replace j cl =
            if j <> i && List.exists (L.contains r) cl then
              (* Replace. Keep clauses simplified. *)
              simplify symdb (BatList.map (L.replace r l) cl)
            else
              (* No change. *)
              Some cl in
          let j = ref ~-1 in
          (* Replace more complex term r by simpler l. *)
          let clauses' =
            BatDynArray.filter_map
              (fun cl -> incr j; replace !j cl)
              clauses in
          loop clauses' in

  loop (simplify_all symdb clauses)

(* Note: a flat clause is simplified and its variables are normalized. *)
let flatten symdb lits =

  let nvars = ref 0 in
  let fresh_var () =
    let x = T.var !nvars in
    incr nvars;
    x in

  let pick_func ts =
    Earray.pick
      (function
      | T.Var _ -> None
      | T.Func _ as t -> Some t)
      ts in

  let var_func_ineq lits i lit = match%earr lit with
    | L.Lit (Sh.Neg, s, [| (T.Var _) as x; (T.Func _) as f |])
    | L.Lit (Sh.Neg, s, [| (T.Func _) as f; (T.Var _) as x |])
      when s = S.sym_eq ->
        if Elist.existsi (fun j l -> j <> i && L.contains f l) lits
        then Some (i, (x, f))
        else None
    | _ -> None in

  let nested_func lit = match%earr lit with
    (* f(..) != g(..) *)
    | L.Lit (Sh.Neg, s, [| (T.Func _) as t; T.Func _ |])
      when s = S.sym_eq ->
        Some t
    (* g(..,f(..),..) ?= t *)
    | L.Lit (_, s, [| l; r |])
      when s = S.sym_eq ->
        begin match pick_func (T.get_args l) with
          | None -> pick_func (T.get_args r)
          | t -> t
        end
    (* ?p(..,f(..),..) *)
    | L.Lit (_, s, args)
      when s <> S.sym_eq ->
        pick_func args
    | _ -> None in

  let func_eq lit = match%earr lit with
    | L.Lit (Sh.Pos, s, [| (T.Func _) as l; (T.Func _) as r |])
      when s = S.sym_eq ->
        Some (l, r)
    | _ -> None in

  let rec loop lits =
    let lits' =
      (* Find a literal [x != f(..)] and replace [f(..)] by [x]
         in other literals.
      *)
      match Elist.picki (var_func_ineq lits) lits with
        | Some (i, (x, f)) ->
            let replace_func j l = if j <> i then L.replace f x l else l in
            Some (BatList.mapi replace_func lits)
        | None ->
      (* Find [f(..)] in [f(..) != g(..)] or [g(..,f(..),..)] or
         [?p(..,f(..),..)] and add a new literal [x != f(..)]
         where [x] is a fresh variable.
      *)
      match Elist.pick nested_func lits with
        | Some f ->
            let ineq = L.mk_ineq (fresh_var ()) f in
            Some (ineq :: lits)
        | None ->
      (* Find all literals [fi(..) = gi(..)] and compute their vertex cover.
         Then add a new literal [xj != hj(..)] for every term [hj(..)] in their
         vertex cover ([xj] is a fresh variable for each [j]).
      *)
      match BatList.filter_map func_eq lits with
        | (_ :: _) as xs ->
            let cover = Algo.min_vertex_cover (Earray.of_list xs) in
            let ineqs =
              BatList.map (fun f -> L.mk_ineq (fresh_var ()) f) cover in
            Some (List.rev_append ineqs lits)
        | [] -> None in
    match lits' with
      (* Clause [lits] is flat. *)
      | None -> Some lits
      | Some lits ->
          match simplify symdb lits with
            | None -> None
            | Some lits -> loop lits in

  match simplify symdb lits with
    | None -> None
    | Some lits ->
        let lits, n = normalize_vars lits in
        nvars := n;
        loop lits

let unflatten symdb lits =
  let var_func_ineq lit = match%earr lit with
    | L.Lit (Sh.Neg, s, [| (T.Var _) as x; (T.Func _) as f |])
    | L.Lit (Sh.Neg, s, [| (T.Func _) as f; (T.Var _) as x |])
      when s = S.sym_eq && not (T.contains x f) ->
        Some (x, f)
    | _ -> None in

  let rec loop lits =
    (* Find and remove a literal [x != f(..)] where [f(..)] doesn't
       contain [x] and replace [x] by [f(..)] in the remaining literals.
    *)
    match Elist.pick_and_remove var_func_ineq lits with
      | None, _ -> Some lits
      | Some (x, f), lits ->
          let lits2 = BatList.map (L.replace x f) lits in
          match simplify symdb lits2 with
            | None -> None
            | Some lits3 -> loop lits3 in

  match simplify symdb lits with
    | None -> None
    | Some lits -> loop lits

module IntSet = Sh.IntSet

let vars lits =
  List.fold_left (fun xs -> L.vars %> IntSet.union xs) IntSet.empty lits

let show lits =
  let lits_str =
    lits
    |> BatList.map L.show
    |> String.concat " | " in
  Printf.sprintf "[%s]" lits_str
