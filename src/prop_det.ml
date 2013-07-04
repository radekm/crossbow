(* Copyright (c) 2013 Radek Micek *)

module T = Term
module L = Lit
module C = Clause

let detect_commutativity symdb clauses =
  BatDynArray.filter_map
    (fun cl ->
      match C.unflatten symdb cl with
        | None -> None
        | Some cl2 ->
            let cl2, _ = C.normalize_vars cl2 in
            match cl2 with
              | [ L.Lit (Sh.Pos, eq,
                         [|
                           T.Func (f, [| T.Var 0; T.Var 1 |]);
                           T.Func (f', [| T.Var 1; T.Var 0 |]);
                         |])
                ] when eq = Symb.sym_eq && f = f' ->
                  Symb.set_commutative symdb f true;
                  None
              | _ -> Some cl)
    clauses
