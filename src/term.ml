(* Copyright (c) 2013 Radek Micek *)

module S = Symb

type var = int

type t =
  | Var of var
  | Func of S.id * t array

let mk_eq l r = Func (S.sym_eq, [| l; r; |])

let mk_ineq l r = Func (S.sym_not, [| mk_eq l r |])

let contains subterm term =
  let rec contains = function
    | term when term = subterm -> true
    | Var _ -> false
    | Func (_, args) -> BatArray.exists contains args in
  contains term

let pickp f term =
  let rec pickp parent term =
    match f parent term, term with
      | None, Var _ -> None
      | None, Func (_, args) -> Earray.pick (pickp (Some term)) args
      | t, _ -> t in
  pickp None term

let rec normalize_comm symdb term = match term with
  | Var _ -> term
  | Func (s, [| l; r |]) when S.commutative symdb s ->
      let l = normalize_comm symdb l in
      let r = normalize_comm symdb r in
      let l, r = if l <= r then (l, r) else (r, l) in
      Func (s, [| l; r |])
  | Func (s, args) ->
      let args = Array.map (normalize_comm symdb) args in
      Func (s, args)

let rec replace a b term = match term with
  | _ when a = term -> b
  | Var _ -> term
  | Func (s, args) -> Func (s, Array.map (replace a b) args)
