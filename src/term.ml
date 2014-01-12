(* Copyright (c) 2013 Radek Micek *)

module Array = Earray.Array
module S = Symb

type var = int

module Inner : sig
  type t = private
    | Var of var
    | Func of S.id * (t, [`R]) Earray.t
  val var : var -> t
  val func : S.id * (t, [> `R]) Earray.t -> t
end = struct
  type t =
    | Var of var
    | Func of S.id * (t, [`R]) Earray.t
  let var x = Var x
  let func (s, args) =
    match Symb.kind s with
      | Symb.Pred -> failwith "func: kind"
      | Symb.Func ->
          if Symb.arity s <> Earray.length args then
            failwith "func: arity"
          else
            Func (s, Earray.read_only args)
end

include Inner

let (|>) = BatPervasives.(|>)

let is_var = function
  | Var _ -> true
  | Func _ -> false

let is_func = function
  | Var _ -> false
  | Func _ -> true

let is_const term = match%earr term with
  | Func (_, [| |]) -> true
  | Var _
  | Func _ -> false

let is_proper_func = function
  | Var _ -> false
  | Func (_, args) -> not (Earray.is_empty args)

let get_args = function
  | Var _ -> Earray.empty
  | Func (_, args) -> args

let contains subterm term =
  let rec contains = function
    | term when term = subterm -> true
    | Var _ -> false
    | Func (_, args) -> Earray.exists contains args in
  contains term

let rec is_ground = function
  | Var _ -> false
  | Func (_, args) -> Earray.for_all is_ground args

let rec iter f term =
  f term;
  match term with
    | Var _ -> ()
    | Func (_, args) -> Earray.iter (iter f) args

let count_symbs term =
  let n = ref 0 in
  iter
    (function
    | Var _ -> ()
    | Func _ -> incr n)
    term;
  !n

let rec normalize_comm symdb term = match%earr term with
  | Var _ -> term
  | Func (s, [| l; r |]) when S.commutative symdb s ->
      let l = normalize_comm symdb l in
      let r = normalize_comm symdb r in
      let l, r = if l <= r then (l, r) else (r, l) in
      func (s, Earray.of_array [| l; r |])
  | Func (s, args) ->
      let args = Earray.map (normalize_comm symdb) args in
      func (s, args)

let rec replace a b term = match term with
  | _ when a = term -> b
  | Var _ -> term
  | Func (s, args) -> func (s, Earray.map (replace a b) args)

module IntSet = Sh.IntSet

let vars term =
  let xs = ref IntSet.empty in
  iter
    (function
    | Var x -> xs := IntSet.add x !xs
    | Func _ -> ())
    term;
  !xs

let rec show = function
  | Var x -> Printf.sprintf "X%d" x
  | Func (f, args) ->
      let args_str =
        args
        |> Earray.map show
        |> Earray.to_list
        |> String.concat ", " in
      Printf.sprintf "f%d(%s)" (Symb.id_to_int f) args_str
