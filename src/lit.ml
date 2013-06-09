(* Copyright (c) 2013 Radek Micek *)

module S = Symb
module T = Term

type 's t =
  | Lit of Sh.sign * 's Symb.id * 's Term.t array

let (|>) = BatPervasives.(|>)
let (|-) = BatPervasives.(|-)

let mk_eq l r = Lit (Sh.Pos, S.sym_eq, [| l; r; |])

let mk_ineq l r = Lit (Sh.Neg, S.sym_eq, [| l; r; |])

let neg (Lit (sign, s, a)) = Lit (Sh.neg sign, s, a)

let is_true = function
  | Lit (Sh.Pos, s, [| l; r |]) -> s = S.sym_eq && l = r
  | _ -> false

let is_false = function
  | Lit (Sh.Neg, s, [| l; r |]) -> s = S.sym_eq && l = r
  | _ -> false

let contains subterm (Lit (_, _, args)) =
  BatArray.exists (T.contains subterm) args

let iter f (Lit (_, _, args)) = Array.iter (T.iter f) args

module IntSet = BatSet.IntSet

let vars (Lit (_, _, args)) =
  Array.fold_left (fun xs -> T.vars |- IntSet.union xs) IntSet.empty args

let lift f (Lit (sign, s, args)) = Lit (sign, s, Array.map f args)

let normalize_comm symdb lit =
  let lit2 = lift (T.normalize_comm symdb) lit in
  match lit2 with
    | Lit (sign, p, [| l; r |]) when Symb.commutative symdb p && l > r ->
        Lit (sign, p, [| r; l |])
    | Lit _ -> lit2

let replace a b = lift (T.replace a b)

let show = function
  | Lit (sign, s, [| l; r |])
    when s = Symb.sym_eq ->
      let op =
        match sign with
          | Sh.Pos -> "="
          | Sh.Neg -> "<>" in
      Printf.sprintf "%s %s %s" (T.show l) op (T.show r)
  | Lit (sign, s, args) ->
      let sign_str =
        match sign with
          | Sh.Pos -> ""
          | Sh.Neg -> "~" in
      let args_str =
        args
        |> Array.map T.show
        |> Array.to_list
        |> String.concat ", " in
      Printf.sprintf "%sp%d(%s)" sign_str (Symb.id_to_int s) args_str
