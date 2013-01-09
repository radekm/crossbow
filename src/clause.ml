(* Copyright (c) 2013 Radek Micek *)

open BatPervasives

module S = Symb
module T = Term

type lit = T.t

type t = {
  cl_id : int;
  cl_lits : lit list;
}

let neg_lit = function
  | T.Func (s, [| t |]) when s = S.sym_not -> t
  | t -> T.Func (S.sym_not, [| t |])

let true_lit = function
  | T.Func (s, [| l; r |]) -> s = S.sym_eq && l = r
  | _ -> false

let false_lit = function
  | T.Func (s, [| T.Func (s2, [| l; r; |]) |]) ->
      s = S.sym_not && s2 = S.sym_eq && l = r
  | _ -> false

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

let simplify symdb cl =
  let lits =
    cl.cl_lits
    |> remove_var_ineqs
    |> BatList.map (T.normalize_comm symdb)
    |> BatList.unique
    |> BatList.filter (false_lit |- not) in
  let taut =
    List.exists (fun l -> true_lit l || Elist.contains (neg_lit l) lits) lits in
  if taut then None else Some { cl with cl_lits = lits }

let normalize_vars cl =
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
  let cl = { cl with cl_lits = BatList.map norm_vars_in_term cl.cl_lits } in
  cl, Hashtbl.length vars
