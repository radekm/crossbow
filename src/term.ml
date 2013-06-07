(* Copyright (c) 2013 Radek Micek *)

module S = Symb

type var = int

type 's t =
  | Var of var
  | Func of 's S.id * 's t array

let (|>) = BatPervasives.(|>)

type 's lit = 's t

let mk_eq l r = Func (S.sym_eq, [| l; r; |])

let mk_ineq l r = Func (S.sym_not, [| mk_eq l r |])

let neg_lit = function
  | Func (s, [| t |]) when s = S.sym_not -> t
  | t -> Func (S.sym_not, [| t |])

let true_lit = function
  | Func (s, [| l; r |]) -> s = S.sym_eq && l = r
  | _ -> false

let false_lit = function
  | Func (s, [| Func (s2, [| l; r; |]) |]) ->
      s = S.sym_not && s2 = S.sym_eq && l = r
  | _ -> false

let contains subterm term =
  let rec contains = function
    | term when term = subterm -> true
    | Var _ -> false
    | Func (_, args) -> BatArray.exists contains args in
  contains term

let rec iter f term =
  f term;
  match term with
    | Var _ -> ()
    | Func (_, args) -> Array.iter (iter f) args

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

module IntSet = BatSet.IntSet

let vars term =
  let xs = ref IntSet.empty in
  iter
    (function
    | Var x -> xs := IntSet.add x !xs
    | Func _ -> ())
    term;
  !xs

let vars_of_many terms =
  List.fold_left
    (fun xs t -> IntSet.union xs (vars t))
    IntSet.empty
    terms

let rec show = function
  | Var x -> Printf.sprintf "X%d" x
  | Func (f, args) ->
      let args_str =
        args
        |> Array.map show
        |> Array.to_list
        |> String.concat ", " in
      Printf.sprintf "f%d(%s)" (Symb.id_to_int f) args_str
