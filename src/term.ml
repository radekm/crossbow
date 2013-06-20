(* Copyright (c) 2013 Radek Micek *)

module S = Symb

type var = int

module Inner : sig
  type 's t = private
    | Var of var
    | Func of 's S.id * 's t array
  val var : var -> 's t
  val func : 's Symb.id * 's t array -> 's t
end = struct
  type 's t =
    | Var of var
    | Func of 's S.id * 's t array
  let var x = Var x
  let func (s, args) =
    match Symb.kind s with
      | Symb.Pred -> failwith "func: kind"
      | Symb.Func ->
          if Symb.arity s <> Array.length args then
            failwith "func: arity"
          else
            Func (s, args)
end

include Inner

let (|>) = BatPervasives.(|>)

let is_const = function
  | Func (_, [| |]) -> true
  | Var _
  | Func _ -> false

let is_proper_func = function
  | Var _
  | Func (_, [| |]) -> false
  | Func _ -> true

let get_args = function
  | Var _ -> [| |]
  | Func (_, args) -> args

let contains subterm term =
  let rec contains = function
    | term when term = subterm -> true
    | Var _ -> false
    | Func (_, args) -> BatArray.exists contains args in
  contains term

let rec is_ground = function
  | Var _ -> false
  | Func (_, args) -> BatArray.for_all is_ground args

let rec iter f term =
  f term;
  match term with
    | Var _ -> ()
    | Func (_, args) -> Array.iter (iter f) args

let count_symbs term =
  let n = ref 0 in
  iter
    (function
    | Var _ -> ()
    | Func _ -> incr n)
    term;
  !n

let rec normalize_comm symdb term = match term with
  | Var _ -> term
  | Func (s, [| l; r |]) when S.commutative symdb s ->
      let l = normalize_comm symdb l in
      let r = normalize_comm symdb r in
      let l, r = if l <= r then (l, r) else (r, l) in
      func (s, [| l; r |])
  | Func (s, args) ->
      let args = Array.map (normalize_comm symdb) args in
      func (s, args)

let rec replace a b term = match term with
  | _ when a = term -> b
  | Var _ -> term
  | Func (s, args) -> func (s, Array.map (replace a b) args)

module IntSet = BatSet.IntSet

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
        |> Array.map show
        |> Array.to_list
        |> String.concat ", " in
      Printf.sprintf "f%d(%s)" (Symb.id_to_int f) args_str
