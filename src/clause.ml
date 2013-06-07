(* Copyright (c) 2013 Radek Micek *)

module S = Symb
module T = Term

type 's t = 's T.lit list

let (|>) = BatPervasives.(|>)
let (|-) = BatPervasives.(|-)

(* Every inequality of variables [x != y] is removed and [x]
   is replaced by [y].
*)
let remove_var_ineqs lits =
  let var_ineq = function
    | T.Func (s, [| T.Func (s2, [| (T.Var _) as x; (T.Var _) as y |]) |])
      when s = S.sym_not && s2 = S.sym_eq ->
        Some (x, y)
    | _ -> None in
  let rec loop lits =
    match Elist.pick_and_remove var_ineq lits with
      | None, _ -> lits
      | Some (x, y), lits ->
          lits
          |> BatList.map (T.replace x y)
          |> loop in
  loop lits

let simplify symdb lits =
  let lits =
    lits
    |> remove_var_ineqs
    |> BatList.map (T.normalize_comm symdb)
    |> BatList.unique
    |> BatList.filter (T.false_lit |- not) in
  let taut =
    List.exists
      (fun l -> T.true_lit l || Elist.contains (T.neg_lit l) lits)
      lits in
  if taut then None else Some lits

let normalize_vars lits =
  let vars = Hashtbl.create 20 in
  let norm_var x =
    if Hashtbl.mem vars x then
      Hashtbl.find vars x
    else
      let y = Hashtbl.length vars in
      let _ = Hashtbl.add vars x y in
      y in
  let rec norm_vars_in_term = function
    | T.Var x -> T.Var (norm_var x)
    | T.Func (s, args) -> T.Func (s, Array.map norm_vars_in_term args) in
  let lits2 = BatList.map norm_vars_in_term lits in
  lits2, Hashtbl.length vars

(* Note: a flat clause is simplified and its variables are normalized. *)
let flatten symdb lits =

  let nvars = ref 0 in
  let fresh_var () =
    let x = T.Var !nvars in
    incr nvars;
    x in

  let var_func_ineq lits i = function
    | T.Func (s, [| T.Func (s2, [| (T.Var _) as x; (T.Func (_, _)) as f |]) |])
    | T.Func (s, [| T.Func (s2, [| (T.Func (_, _)) as f; (T.Var _) as x |]) |])
      when s = S.sym_not && s2 = S.sym_eq ->
        if Elist.existsi (fun j l -> j <> i && Term.contains f l) lits
        then Some (i, (x, f))
        else None
    | _ -> None in

  let nested_func p t =
    match p, t with
      (* f(..) != g(..) *)
      | Some (T.Func (s, _)), T.Func (s2, [| (T.Func (_, _)) as t; T.Func (_, _) |])
        when s = S.sym_not && s2 = S.sym_eq -> Some t
      (* g(..,f(..),..) or ?p(..,f(..),..) *)
      | Some (T.Func (s, _)), T.Func (_, _)
        when s <> S.sym_not && s <> S.sym_eq -> Some t
      | _ -> None in

  let func_eq = function
    | T.Func (s, [| (T.Func (_, _)) as l; (T.Func (_, _)) as r |])
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
            let replace_func j l = if j <> i then Term.replace f x l else l in
            Some (BatList.mapi replace_func lits)
        | None ->
      (* Find [f(..)] in [f(..) != g(..)] or [g(..,f(..),..)] or
         [?p(..,f(..),..)] and add a new literal [x != f(..)]
         where [x] is a fresh variable.
      *)
      match Elist.pick (Term.pickp nested_func) lits with
        | Some f ->
            let ineq = Term.mk_ineq (fresh_var ()) f in
            Some (ineq :: lits)
        | None ->
      (* Find all literals [fi(..) = gi(..)] and compute their vertex cover.
         Then add a new literal [xj != hj(..)] for every term [hj(..)] in their
         vertex cover ([xj] is a fresh variable for each [j]).
      *)
      match BatList.filter_map func_eq lits with
        | (_ :: _) as xs ->
            let cover = Algo.min_vertex_cover (Array.of_list xs) in
            let ineqs = BatList.map (fun f -> Term.mk_ineq (fresh_var ()) f) cover in
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
  let var_func_ineq = function
    | T.Func (s, [| T.Func (s2, [| (T.Var _) as x; (T.Func (_, _)) as f |]) |])
    | T.Func (s, [| T.Func (s2, [| (T.Func (_, _)) as f; (T.Var _) as x |]) |])
      when s = S.sym_not && s2 = S.sym_eq && not (Term.contains x f) ->
        Some (x, f)
    | _ -> None in

  let rec loop lits =
    (* Find and remove a literal [x != f(..)] where [f(..)] doesn't
       contain [x] and replace [x] by [f(..)] in the remaining literals.
    *)
    match Elist.pick_and_remove var_func_ineq lits with
      | None, _ -> Some lits
      | Some (x, f), lits ->
          let lits2 = BatList.map (Term.replace x f) lits in
          match simplify symdb lits2 with
            | None -> None
            | Some lits3 -> loop lits3 in

  match simplify symdb lits with
    | None -> None
    | Some lits -> loop lits

module IntSet = BatSet.IntSet

let vars terms =
  List.fold_left
    (fun xs t -> IntSet.union xs (T.vars t))
    IntSet.empty
    terms

let show lits =
  let lits_str =
    lits
    |> BatList.map T.show
    |> String.concat "; " in
  Printf.sprintf "[%s]" lits_str
