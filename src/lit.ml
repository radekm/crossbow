(* Copyright (c) 2013 Radek Micek *)

module Array = Earray.Array
module S = Symb
module T = Term

module Inner : sig
  type t = private
    | Lit of Sh.sign * S.id * (T.t, [`R]) Earray.t
  val lit : Sh.sign * S.id * (T.t, [> `R]) Earray.t -> t
end = struct
  type t =
    | Lit of Sh.sign * S.id * (T.t, [`R]) Earray.t
  let lit (sign, s, args) =
    match S.kind s with
      | S.Func -> failwith "lit: kind"
      | S.Pred ->
          if S.arity s <> Earray.length args then
            failwith "lit: arity"
          else
            Lit (sign, s, Earray.read_only args)
end

include Inner

let (|>) = BatPervasives.(|>)
let (%>) = BatPervasives.(%>)

let mk_eq l r = lit (Sh.Pos, S.sym_eq, Earray.of_array [| l; r; |])

let mk_ineq l r = lit (Sh.Neg, S.sym_eq, Earray.of_array [| l; r; |])

let neg (Lit (sign, s, a)) = lit (Sh.neg sign, s, a)

let is_true lit = match%earr lit with
  | Lit (Sh.Pos, s, [| l; r |]) -> s = S.sym_eq && l = r
  | _ -> false

let is_false lit = match%earr lit with
  | Lit (Sh.Neg, s, [| l; r |]) -> s = S.sym_eq && l = r
  | _ -> false

let contains subterm (Lit (_, _, args)) =
  Earray.exists (T.contains subterm) args

let iter f (Lit (_, _, args)) = Earray.iter (T.iter f) args

module IntSet = Sh.IntSet

let vars (Lit (_, _, args)) =
  Earray.fold_left (fun xs -> T.vars %> IntSet.union xs) IntSet.empty args

let lift f (Lit (sign, s, args)) = lit (sign, s, Earray.map f args)

let normalize_comm symdb lit1 =
  let lit2 = lift (T.normalize_comm symdb) lit1 in
  match%earr lit2 with
    | Lit (sign, p, [| l; r |]) when Symb.commutative symdb p && l > r ->
        lit (sign, p, Earray.of_array [| r; l |])
    | Lit _ -> lit2

let replace a b = lift (T.replace a b)

let show lit = match%earr lit with
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
        |> Earray.map T.show
        |> Earray.to_list
        |> String.concat ", " in
      Printf.sprintf "%sp%d(%s)" sign_str (Symb.id_to_int s) args_str
